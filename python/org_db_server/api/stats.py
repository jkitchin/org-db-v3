"""Statistics API endpoints."""
from fastapi import APIRouter, HTTPException
from typing import Dict, Any
import os
import logging

from org_db_server.services.database import Database
from org_db_server.config import settings

logger = logging.getLogger(__name__)

router = APIRouter(prefix="/api/stats", tags=["stats"])

# Global database instance
db = Database(settings.db_path)

@router.get("/", response_model=Dict[str, Any])
async def get_stats():
    """Get database statistics."""
    cursor = db.conn.cursor()

    stats = {}

    # Org file count
    cursor.execute("SELECT COUNT(*) FROM files")
    stats["files_count"] = cursor.fetchone()[0]
    stats["org_files_count"] = stats["files_count"]  # Alias for clarity

    # Linked files count (PDF, DOCX, etc.)
    cursor.execute("SELECT COUNT(*) FROM linked_files")
    stats["linked_files_count"] = cursor.fetchone()[0]

    # Headline count
    cursor.execute("SELECT COUNT(*) FROM headlines")
    stats["headlines_count"] = cursor.fetchone()[0]

    # Link count
    cursor.execute("SELECT COUNT(*) FROM links")
    stats["links_count"] = cursor.fetchone()[0]

    # Chunk count
    cursor.execute("SELECT COUNT(*) FROM chunks")
    stats["chunks_count"] = cursor.fetchone()[0]

    # Embedding count
    cursor.execute("SELECT COUNT(*) FROM embeddings")
    stats["embeddings_count"] = cursor.fetchone()[0]

    # Image count
    cursor.execute("SELECT COUNT(*) FROM images")
    stats["images_count"] = cursor.fetchone()[0]

    # Image embedding count
    cursor.execute("SELECT COUNT(*) FROM image_embeddings")
    stats["image_embeddings_count"] = cursor.fetchone()[0]

    # FTS content count
    cursor.execute("SELECT COUNT(*) FROM fts_content")
    stats["fts_entries_count"] = cursor.fetchone()[0]

    # Recent files (last 5)
    cursor.execute("""
        SELECT filename, indexed_at
        FROM files
        ORDER BY indexed_at DESC
        LIMIT 5
    """)
    stats["recent_files"] = [
        {"filename": row[0], "indexed_at": row[1]}
        for row in cursor.fetchall()
    ]

    # Database file size and location
    import os
    stats["db_path"] = str(settings.db_path)
    if os.path.exists(settings.db_path):
        stats["db_size_bytes"] = os.path.getsize(settings.db_path)
        stats["db_size_mb"] = round(stats["db_size_bytes"] / (1024 * 1024), 2)
    else:
        stats["db_size_bytes"] = 0
        stats["db_size_mb"] = 0

    return stats

@router.get("/files", response_model=Dict[str, Any])
async def get_files():
    """Get all files in the database."""
    cursor = db.conn.cursor()

    cursor.execute("""
        SELECT filename, indexed_at
        FROM files
        ORDER BY indexed_at DESC
    """)

    files = [
        {"filename": row[0], "indexed_at": row[1]}
        for row in cursor.fetchall()
    ]

    return {"files": files, "count": len(files)}

@router.delete("/clear-database", response_model=Dict[str, Any])
async def clear_database():
    """Clear the entire database by removing the database file.

    WARNING: This is destructive and cannot be undone!
    All indexed data will be permanently deleted.
    """
    global db

    db_path = settings.db_path

    if not db_path.exists():
        raise HTTPException(status_code=404, detail="Database file does not exist")

    try:
        # Close the database connection first
        db.conn.close()
        logger.info(f"Closed database connection for {db_path}")

        # Delete the database file
        os.remove(db_path)
        logger.info(f"Deleted database file: {db_path}")

        # Reinitialize the database with a fresh connection
        db = Database(db_path)
        logger.info(f"Reinitialized empty database at {db_path}")

        return {
            "status": "success",
            "message": "Database cleared successfully",
            "db_path": str(db_path)
        }

    except Exception as e:
        logger.error(f"Error clearing database: {str(e)}")
        raise HTTPException(status_code=500, detail=f"Failed to clear database: {str(e)}")
