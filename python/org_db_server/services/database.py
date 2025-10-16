"""Database service for org-db with multi-database architecture.

Three separate databases:
1. Main DB: Metadata, headlines, links, properties, FTS5
2. Semantic DB: Text chunks and embeddings with vector search
3. Image DB: Images and CLIP embeddings with vector search
"""
import libsql
from pathlib import Path
from typing import Optional, List, Dict
from datetime import datetime
import numpy as np
import logging

from org_db_server.models.db_models import SCHEMA
from org_db_server.models.semantic_schema import SEMANTIC_SCHEMA
from org_db_server.models.image_schema import IMAGE_SCHEMA
from org_db_server.migrations import run_migrations

logger = logging.getLogger(__name__)


def _row_to_dict(cursor, row):
    """Convert a database row tuple to a dict using cursor description."""
    if row is None:
        return None
    return {desc[0]: value for desc, value in zip(cursor.description, row)}


class Database:
    """Database connections and operations across three databases."""

    def __init__(self, main_path: Path, semantic_path: Path, image_path: Path):
        """Initialize three database connections and create schemas if needed.

        Args:
            main_path: Path to main database (metadata, FTS5)
            semantic_path: Path to semantic search database (chunks, embeddings)
            image_path: Path to image search database (images, CLIP embeddings)
        """
        self.main_path = main_path
        self.semantic_path = semantic_path
        self.image_path = image_path

        # Connect to all three databases
        self.main_conn = libsql.connect(str(main_path))
        self.semantic_conn = libsql.connect(str(semantic_path))
        self.image_conn = libsql.connect(str(image_path))

        # Enable foreign keys on all connections
        self.main_conn.execute("PRAGMA foreign_keys = ON")
        self.semantic_conn.execute("PRAGMA foreign_keys = ON")
        self.image_conn.execute("PRAGMA foreign_keys = ON")

        # Check if main DB is new
        cursor = self.main_conn.cursor()
        cursor.execute("SELECT name FROM sqlite_master WHERE type='table' AND name='files'")
        is_new_main = cursor.fetchone() is None

        # Initialize schemas
        if is_new_main:
            self._initialize_main_schema()
            # Mark migrations as applied for new DB
            from org_db_server.migrations import get_schema_version, set_schema_version, MIGRATIONS
            if get_schema_version(self.main_conn) == 0 and MIGRATIONS:
                latest_version = max(m[0] for m in MIGRATIONS)
                set_schema_version(self.main_conn, latest_version)
                logger.info(f"New main database created with schema version {latest_version}")
        else:
            # Existing database: run migrations first
            run_migrations(self.main_conn)
            self._initialize_main_schema()

        # Always initialize semantic and image schemas (they're new or need creation)
        self._initialize_semantic_schema()
        self._initialize_image_schema()

        logger.info(f"Initialized multi-database: main={main_path.name}, semantic={semantic_path.name}, image={image_path.name}")

    def _initialize_main_schema(self):
        """Create main database tables (metadata, FTS5)."""
        cursor = self.main_conn.cursor()
        try:
            cursor.executescript(SCHEMA)
            self.main_conn.commit()
            logger.debug("Main schema initialized")
        except Exception as e:
            logger.error(f"Error initializing main schema: {e}")
            raise

    def _initialize_semantic_schema(self):
        """Create semantic database tables (chunks, embeddings, vector index)."""
        cursor = self.semantic_conn.cursor()
        try:
            cursor.executescript(SEMANTIC_SCHEMA)
            self.semantic_conn.commit()
            logger.debug("Semantic schema initialized")
        except ValueError as e:
            if "vector index" in str(e) and "unexpected vector column type" in str(e):
                # Vector indexes already exist, this is fine
                logger.info("Skipping vector index creation in semantic DB - already exists")
                self.semantic_conn.commit()
            else:
                raise
        except Exception as e:
            logger.error(f"Error initializing semantic schema: {e}")
            raise

    def _initialize_image_schema(self):
        """Create image database tables (images, CLIP embeddings, vector index)."""
        cursor = self.image_conn.cursor()
        try:
            cursor.executescript(IMAGE_SCHEMA)
            self.image_conn.commit()
            logger.debug("Image schema initialized")
        except ValueError as e:
            if "vector index" in str(e) and "unexpected vector column type" in str(e):
                # Vector indexes already exist, this is fine
                logger.info("Skipping vector index creation in image DB - already exists")
                self.image_conn.commit()
            else:
                raise
        except Exception as e:
            logger.error(f"Error initializing image schema: {e}")
            raise

    def close(self):
        """Close all database connections."""
        if self.main_conn:
            self.main_conn.close()
        if self.semantic_conn:
            self.semantic_conn.close()
        if self.image_conn:
            self.image_conn.close()
        logger.info("Closed all database connections")

    def optimize(self):
        """Optimize all databases by running ANALYZE.

        This improves query performance, especially for vector searches.
        Should be run periodically after bulk indexing operations.
        """
        # Optimize main DB
        cursor = self.main_conn.cursor()
        cursor.execute("ANALYZE")
        self.main_conn.commit()
        logger.info("Main database optimized (ANALYZE completed)")

        # Optimize semantic DB
        cursor = self.semantic_conn.cursor()
        cursor.execute("ANALYZE")
        self.semantic_conn.commit()
        logger.info("Semantic database optimized (ANALYZE completed)")

        # Optimize image DB
        cursor = self.image_conn.cursor()
        cursor.execute("ANALYZE")
        self.image_conn.commit()
        logger.info("Image database optimized (ANALYZE completed)")

    # -------------------------------------------------------------------------
    # Main DB Operations (metadata, headlines, links, FTS5)
    # -------------------------------------------------------------------------

    def get_or_create_file_id(self, filename: str, md5: str, file_size: int) -> int:
        """Get file ID or create new file entry in main DB."""
        cursor = self.main_conn.cursor()

        # Try to get existing file
        cursor.execute("SELECT rowid FROM files WHERE filename = ?", (filename,))
        row = cursor.fetchone()

        if row:
            # Update existing file
            cursor.execute(
                """UPDATE files SET md5 = ?, last_updated = ?, file_size = ?, indexed_at = ?
                   WHERE rowid = ?""",
                (md5, datetime.now().isoformat(), file_size, datetime.now().isoformat(), row[0])
            )
            self.main_conn.commit()
            return row[0]
        else:
            # Create new file
            cursor.execute(
                """INSERT INTO files (filename, md5, last_updated, file_size, indexed_at)
                   VALUES (?, ?, ?, ?, ?)""",
                (filename, md5, datetime.now().isoformat(), file_size, datetime.now().isoformat())
            )
            self.main_conn.commit()
            return cursor.lastrowid

    def populate_fts(self, filename_id: int, filename: str, content: str):
        """Populate FTS5 table with entire file content in main DB."""
        cursor = self.main_conn.cursor()

        # Delete existing FTS entries for this file
        cursor.execute(
            "DELETE FROM fts_content WHERE filename = ?",
            (filename,)
        )

        # Insert entire file content into FTS
        cursor.execute(
            "INSERT INTO fts_content(filename, title, content, tags) VALUES (?, ?, ?, ?)",
            (filename, filename, content, "")
        )

        self.main_conn.commit()

    def get_or_create_linked_file(
        self,
        org_file_id: int,
        org_link_line: int,
        file_path: str,
        file_type: str,
        file_size: int,
        md5: str,
        conversion_status: str = 'pending',
        conversion_error: Optional[str] = None
    ) -> int:
        """Get linked file ID or create new linked file entry in main DB."""
        cursor = self.main_conn.cursor()

        # Try to get existing linked file entry
        cursor.execute(
            """SELECT rowid, md5, conversion_status FROM linked_files
               WHERE org_file_id = ? AND org_link_line = ? AND file_path = ?""",
            (org_file_id, org_link_line, file_path)
        )
        row = cursor.fetchone()

        now = datetime.now().isoformat()

        if row:
            linked_file_id, old_md5, old_status = row

            # Update if MD5 changed or status changed
            if old_md5 != md5 or old_status != conversion_status:
                cursor.execute(
                    """UPDATE linked_files
                       SET md5 = ?, file_size = ?, last_converted = ?,
                           conversion_status = ?, conversion_error = ?, indexed_at = ?
                       WHERE rowid = ?""",
                    (md5, file_size, now, conversion_status, conversion_error, now, linked_file_id)
                )
                self.main_conn.commit()
                logger.debug(f"Updated linked file {file_path} (id={linked_file_id})")

            return linked_file_id
        else:
            # Create new linked file entry
            cursor.execute(
                """INSERT INTO linked_files
                   (org_file_id, org_link_line, file_path, file_type, file_size, md5,
                    last_converted, conversion_status, conversion_error, indexed_at)
                   VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)""",
                (org_file_id, org_link_line, file_path, file_type, file_size, md5,
                 now, conversion_status, conversion_error, now)
            )
            self.main_conn.commit()
            linked_file_id = cursor.lastrowid
            logger.debug(f"Created linked file entry for {file_path} (id={linked_file_id})")
            return linked_file_id

    def get_linked_file_info(self, linked_file_id: int) -> Optional[Dict]:
        """Get information about a linked file from main DB."""
        cursor = self.main_conn.cursor()
        cursor.execute(
            """SELECT lf.*, f.filename as org_filename
               FROM linked_files lf
               JOIN files f ON lf.org_file_id = f.rowid
               WHERE lf.rowid = ?""",
            (linked_file_id,)
        )
        row = cursor.fetchone()
        return _row_to_dict(cursor, row)

    def get_linked_files_for_org_file(self, org_file_id: int) -> List[Dict]:
        """Get all linked files for an org file from main DB."""
        cursor = self.main_conn.cursor()

        # Note: Can't JOIN with chunks from semantic DB
        # Just return linked file metadata
        cursor.execute(
            """SELECT lf.*
               FROM linked_files lf
               WHERE lf.org_file_id = ?
               ORDER BY lf.org_link_line""",
            (org_file_id,)
        )
        return [_row_to_dict(cursor, row) for row in cursor.fetchall()]

    # -------------------------------------------------------------------------
    # Semantic DB Operations (chunks, embeddings, vector search)
    # -------------------------------------------------------------------------

    def store_chunks(self, filename: str, chunks: List[Dict], embeddings: List[np.ndarray], model_name: str):
        """Store text chunks and their embeddings in semantic DB.

        Args:
            filename: Full path to org file (used instead of file_id)
            chunks: List of chunk dictionaries
            embeddings: List of embedding vectors
            model_name: Name of embedding model used
        """
        cursor = self.semantic_conn.cursor()

        # Delete existing chunks for this file
        cursor.execute("DELETE FROM chunks WHERE filename = ?", (filename,))

        for chunk_data, embedding in zip(chunks, embeddings):
            # Insert chunk
            cursor.execute(
                """INSERT INTO chunks (filename, headline_id, chunk_text, chunk_type, begin_line, end_line, char_offset, linked_file_id, linked_file_path)
                   VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)""",
                (filename, None, chunk_data["text"], chunk_data["chunk_type"],
                 chunk_data["begin_line"], chunk_data["end_line"], 0, None, None)
            )
            chunk_id = cursor.lastrowid

            # Convert embedding to bytes
            embedding_bytes = embedding.astype(np.float32).tobytes()

            # Insert embedding
            cursor.execute(
                """INSERT INTO embeddings (chunk_id, embedding_model, embedding_vector, embedding_dim, created_at)
                   VALUES (?, ?, ?, ?, ?)""",
                (chunk_id, model_name, embedding_bytes, len(embedding), datetime.now().isoformat())
            )

        self.semantic_conn.commit()

    def store_linked_file_chunks(
        self,
        org_filename: str,
        org_link_line: int,
        linked_file_path: str,
        chunks: List[Dict],
        embeddings: List[np.ndarray],
        model_name: str
    ):
        """Store chunks from a linked file in semantic DB.

        Args:
            org_filename: Full path to org file containing the link
            org_link_line: Line number in org file where link exists
            linked_file_path: Path to the linked file
            chunks: List of chunk dictionaries
            embeddings: List of embedding vectors
            model_name: Name of embedding model used
        """
        cursor = self.semantic_conn.cursor()

        # Delete existing chunks for this linked file
        cursor.execute(
            "DELETE FROM chunks WHERE linked_file_path = ?",
            (linked_file_path,)
        )

        for chunk_data, embedding in zip(chunks, embeddings):
            # Insert chunk pointing to org file location
            cursor.execute(
                """INSERT INTO chunks
                   (filename, headline_id, chunk_text, chunk_type, begin_line, end_line, char_offset, linked_file_id, linked_file_path)
                   VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)""",
                (org_filename, None, chunk_data["text"], 'linked_file',
                 org_link_line, org_link_line, 0, None, linked_file_path)
            )
            chunk_id = cursor.lastrowid

            # Convert embedding to bytes
            embedding_bytes = embedding.astype(np.float32).tobytes()

            # Insert embedding
            cursor.execute(
                """INSERT INTO embeddings (chunk_id, embedding_model, embedding_vector, embedding_dim, created_at)
                   VALUES (?, ?, ?, ?, ?)""",
                (chunk_id, model_name, embedding_bytes, len(embedding), datetime.now().isoformat())
            )

        self.semantic_conn.commit()
        logger.debug(f"Stored {len(chunks)} chunks for linked file {linked_file_path}")

    # -------------------------------------------------------------------------
    # Image DB Operations (images, CLIP embeddings, vector search)
    # -------------------------------------------------------------------------

    def store_images(self, filename: str, images: List[Dict], embeddings: List[np.ndarray], model_name: str):
        """Store images and their CLIP embeddings in image DB.

        Args:
            filename: Full path to org file (used instead of file_id)
            images: List of image dictionaries
            embeddings: List of CLIP embedding vectors
            model_name: Name of CLIP model used
        """
        cursor = self.image_conn.cursor()

        # Delete existing images for this file
        cursor.execute("DELETE FROM images WHERE filename = ?", (filename,))

        for image_data, embedding in zip(images, embeddings):
            # Insert image
            cursor.execute(
                """INSERT INTO images (filename, image_path, begin)
                   VALUES (?, ?, ?)""",
                (filename, image_data["path"], image_data["begin"])
            )
            image_id = cursor.lastrowid

            # Convert embedding to bytes
            embedding_bytes = embedding.astype(np.float32).tobytes()

            # Insert image embedding
            cursor.execute(
                """INSERT INTO image_embeddings (image_id, clip_model, embedding_vector, embedding_dim, created_at)
                   VALUES (?, ?, ?, ?, ?)""",
                (image_id, model_name, embedding_bytes, len(embedding), datetime.now().isoformat())
            )

        self.image_conn.commit()
