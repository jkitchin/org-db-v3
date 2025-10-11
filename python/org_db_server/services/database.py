"""Database service for org-db."""
import sqlite3
from pathlib import Path
from typing import Optional, List, Dict
from datetime import datetime
import numpy as np

from org_db_server.models.db_models import SCHEMA

class Database:
    """Database connection and operations."""

    def __init__(self, db_path: Path):
        """Initialize database connection and create schema if needed."""
        self.db_path = db_path
        self.conn = sqlite3.connect(str(db_path), check_same_thread=False)
        self.conn.row_factory = sqlite3.Row

        # Enable foreign keys
        self.conn.execute("PRAGMA foreign_keys = ON")

        # Initialize schema
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

    def populate_fts(self, filename_id: int, headlines: List[Dict]):
        """Populate FTS5 table with headline content."""
        cursor = self.conn.cursor()

        # Get filename
        cursor.execute("SELECT filename FROM files WHERE rowid = ?", (filename_id,))
        filename_row = cursor.fetchone()
        if not filename_row:
            return

        filename = filename_row[0]

        # Delete existing FTS entries for this file
        cursor.execute(
            "DELETE FROM fts_content WHERE filename = ?",
            (filename,)
        )

        # Insert headlines directly into FTS
        for hl in headlines:
            title = hl.get("title", "")
            tags = hl.get("tags", "")

            if title:  # Only index if there's a title
                cursor.execute(
                    "INSERT INTO fts_content(filename, title, content, tags) VALUES (?, ?, ?, ?)",
                    (filename, title, title, tags)  # Use title as content for now
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
