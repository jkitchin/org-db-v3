"""Database migrations for org-db v3."""
import logging
from typing import List, Callable, Any

logger = logging.getLogger(__name__)


def get_schema_version(conn: Any) -> int:
    """Get current schema version from database."""
    cursor = conn.cursor()

    # Create schema_version table if it doesn't exist
    cursor.execute("""
        CREATE TABLE IF NOT EXISTS schema_version (
            version INTEGER PRIMARY KEY,
            applied_at TEXT NOT NULL
        )
    """)

    # Get current version
    cursor.execute("SELECT MAX(version) FROM schema_version")
    result = cursor.fetchone()
    return result[0] if result[0] is not None else 0


def set_schema_version(conn: Any, version: int):
    """Set schema version in database."""
    from datetime import datetime
    cursor = conn.cursor()
    cursor.execute(
        "INSERT INTO schema_version (version, applied_at) VALUES (?, ?)",
        (version, datetime.now().isoformat())
    )
    conn.commit()


def migration_001_add_linked_files(conn: Any):
    """Migration 001: Add linked_file_id column to chunks table.

    This migration adds support for indexing linked files (PDF, DOCX, etc.)
    by adding a foreign key from chunks to linked_files.
    """
    cursor = conn.cursor()

    # Check if linked_file_id column already exists
    cursor.execute("PRAGMA table_info(chunks)")
    columns = {row[1] for row in cursor.fetchall()}

    if 'linked_file_id' not in columns:
        logger.info("Adding linked_file_id column to chunks table...")
        cursor.execute("""
            ALTER TABLE chunks ADD COLUMN linked_file_id INTEGER
            REFERENCES linked_files(rowid) ON DELETE CASCADE
        """)

        # Create index
        cursor.execute("""
            CREATE INDEX IF NOT EXISTS idx_chunks_linked_file ON chunks(linked_file_id)
        """)

        conn.commit()
        logger.info("Migration 001 completed: linked_file_id column added")
    else:
        logger.info("Migration 001 skipped: linked_file_id column already exists")


# Note: Migration 002 (vector indexes) is now part of the base schema in db_models.py
# No need for separate migration since vector indexes are created with the table

# List of all migrations in order
MIGRATIONS: List[tuple[int, str, Callable[[Any], None]]] = [
    (1, "Add linked_file_id to chunks table", migration_001_add_linked_files),
]


def run_migrations(conn: Any):
    """Run all pending migrations."""
    current_version = get_schema_version(conn)
    logger.info(f"Current database schema version: {current_version}")

    pending = [m for m in MIGRATIONS if m[0] > current_version]

    if not pending:
        logger.info("Database schema is up to date")
        return

    logger.info(f"Running {len(pending)} pending migration(s)...")

    for version, description, migration_func in pending:
        logger.info(f"Running migration {version}: {description}")
        try:
            migration_func(conn)
            set_schema_version(conn, version)
            logger.info(f"Migration {version} completed successfully")
        except Exception as e:
            logger.error(f"Migration {version} failed: {e}", exc_info=True)
            raise

    logger.info("All migrations completed successfully")
