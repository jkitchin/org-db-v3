"""Database service for org-db."""
import libsql
from pathlib import Path
from typing import Optional, List, Dict
from datetime import datetime
import numpy as np
import logging

from org_db_server.models.db_models import SCHEMA
from org_db_server.migrations import run_migrations

logger = logging.getLogger(__name__)


def _row_to_dict(cursor, row):
    """Convert a database row tuple to a dict using cursor description."""
    if row is None:
        return None
    return {desc[0]: value for desc, value in zip(cursor.description, row)}


class Database:
    """Database connection and operations."""

    def __init__(self, db_path: Path):
        """Initialize database connection and create schema if needed."""
        self.db_path = db_path
        # libsql doesn't support check_same_thread parameter
        self.conn = libsql.connect(str(db_path))
        # Note: libsql doesn't support row_factory, we'll handle dict conversion where needed

        # Enable foreign keys
        self.conn.execute("PRAGMA foreign_keys = ON")

        # Check if this is a new database
        cursor = self.conn.cursor()
        cursor.execute("SELECT name FROM sqlite_master WHERE type='table' AND name='files'")
        is_new_db = cursor.fetchone() is None

        if is_new_db:
            # New database: initialize schema first (includes new columns)
            self._initialize_schema()
            # Mark as having all migrations applied
            from org_db_server.migrations import get_schema_version, set_schema_version, MIGRATIONS
            if get_schema_version(self.conn) == 0 and MIGRATIONS:
                latest_version = max(m[0] for m in MIGRATIONS)
                set_schema_version(self.conn, latest_version)
                logger.info(f"New database created with schema version {latest_version}")
        else:
            # Existing database: run migrations first, then schema (for new tables only)
            run_migrations(self.conn)
            self._initialize_schema()

    def _initialize_schema(self):
        """Create all tables if they don't exist."""
        cursor = self.conn.cursor()
        cursor.executescript(SCHEMA)
        self.conn.commit()

    def close(self):
        """Close database connection."""
        if self.conn:
            self.conn.close()

    def get_or_create_file_id(self, filename: str, md5: str, file_size: int) -> int:
        """Get file ID or create new file entry."""
        cursor = self.conn.cursor()

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
            self.conn.commit()
            return row[0]
        else:
            # Create new file
            cursor.execute(
                """INSERT INTO files (filename, md5, last_updated, file_size, indexed_at)
                   VALUES (?, ?, ?, ?, ?)""",
                (filename, md5, datetime.now().isoformat(), file_size, datetime.now().isoformat())
            )
            self.conn.commit()
            return cursor.lastrowid

    def store_chunks(self, filename_id: int, chunks: List[Dict], embeddings: List[np.ndarray], model_name: str):
        """Store text chunks and their embeddings."""
        cursor = self.conn.cursor()

        # Delete existing chunks for this file
        cursor.execute("DELETE FROM chunks WHERE filename_id = ?", (filename_id,))

        for chunk_data, embedding in zip(chunks, embeddings):
            # Insert chunk
            cursor.execute(
                """INSERT INTO chunks (filename_id, headline_id, chunk_text, chunk_type, begin_line, end_line, char_offset)
                   VALUES (?, ?, ?, ?, ?, ?, ?)""",
                (filename_id, None, chunk_data["text"], chunk_data["chunk_type"],
                 chunk_data["begin_line"], chunk_data["end_line"], 0)
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

        self.conn.commit()

    def populate_fts(self, filename_id: int, filename: str, content: str):
        """Populate FTS5 table with entire file content."""
        cursor = self.conn.cursor()

        # Delete existing FTS entries for this file
        cursor.execute(
            "DELETE FROM fts_content WHERE filename = ?",
            (filename,)
        )

        # Insert entire file content into FTS
        # Use filename as title, full content as content, empty tags
        cursor.execute(
            "INSERT INTO fts_content(filename, title, content, tags) VALUES (?, ?, ?, ?)",
            (filename, filename, content, "")
        )

        self.conn.commit()

    def store_images(self, filename_id: int, images: List[Dict], embeddings: List[np.ndarray], model_name: str):
        """Store images and their CLIP embeddings."""
        cursor = self.conn.cursor()

        # Delete existing images for this file
        cursor.execute("DELETE FROM images WHERE filename_id = ?", (filename_id,))

        for image_data, embedding in zip(images, embeddings):
            # Insert image
            cursor.execute(
                """INSERT INTO images (filename_id, image_path, begin)
                   VALUES (?, ?, ?)""",
                (filename_id, image_data["path"], image_data["begin"])
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

        self.conn.commit()

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
        """Get linked file ID or create new linked file entry.

        Args:
            org_file_id: ID of the org file containing the link
            org_link_line: Line number in org file where link exists
            file_path: Path to the linked file
            file_type: File extension/type (pdf, docx, etc)
            file_size: Size of the linked file in bytes
            md5: MD5 hash of the linked file
            conversion_status: Status of document conversion (pending, success, error, skipped)
            conversion_error: Error message if conversion failed

        Returns:
            Row ID of the linked file entry
        """
        cursor = self.conn.cursor()

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
                self.conn.commit()
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
            self.conn.commit()
            linked_file_id = cursor.lastrowid
            logger.debug(f"Created linked file entry for {file_path} (id={linked_file_id})")
            return linked_file_id

    def store_linked_file_chunks(
        self,
        org_file_id: int,
        org_link_line: int,
        linked_file_id: int,
        chunks: List[Dict],
        embeddings: List[np.ndarray],
        model_name: str
    ):
        """Store chunks from a linked file.

        Chunks are stored with:
        - filename_id: Points to the org file (not the linked file)
        - begin_line/end_line: The line in org file where the link exists
        - linked_file_id: Reference to the linked file entry
        - chunk_type: 'linked_file'

        Args:
            org_file_id: ID of the org file containing the link
            org_link_line: Line number in org file where link exists
            linked_file_id: ID of the linked file entry
            chunks: List of chunk dictionaries with 'text' field
            embeddings: List of embedding vectors
            model_name: Name of the embedding model used
        """
        cursor = self.conn.cursor()

        # Delete existing chunks for this linked file
        cursor.execute("DELETE FROM chunks WHERE linked_file_id = ?", (linked_file_id,))

        for chunk_data, embedding in zip(chunks, embeddings):
            # Insert chunk pointing to org file location
            cursor.execute(
                """INSERT INTO chunks
                   (filename_id, headline_id, chunk_text, chunk_type, begin_line, end_line, char_offset, linked_file_id)
                   VALUES (?, ?, ?, ?, ?, ?, ?, ?)""",
                (org_file_id, None, chunk_data["text"], 'linked_file',
                 org_link_line, org_link_line, 0, linked_file_id)
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

        self.conn.commit()
        logger.debug(f"Stored {len(chunks)} chunks for linked file {linked_file_id}")

    def get_linked_file_info(self, linked_file_id: int) -> Optional[Dict]:
        """Get information about a linked file."""
        cursor = self.conn.cursor()
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
        """Get all linked files for an org file with chunk counts."""
        cursor = self.conn.cursor()
        cursor.execute(
            """SELECT lf.*, COUNT(c.rowid) as chunk_count
               FROM linked_files lf
               LEFT JOIN chunks c ON c.linked_file_id = lf.rowid
               WHERE lf.org_file_id = ?
               GROUP BY lf.rowid
               ORDER BY lf.org_link_line""",
            (org_file_id,)
        )
        return [_row_to_dict(cursor, row) for row in cursor.fetchall()]
