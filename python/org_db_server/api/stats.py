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
db = Database(settings.db_path, settings.semantic_db_path, settings.image_db_path)

@router.get("/", response_model=Dict[str, Any])
async def get_stats():
    """Get database statistics from all three databases."""
    stats = {}

    # Main DB stats
    main_cursor = db.main_conn.cursor()

    # Org file count
    main_cursor.execute("SELECT COUNT(*) FROM files")
    stats["files_count"] = main_cursor.fetchone()[0]
    stats["org_files_count"] = stats["files_count"]  # Alias for clarity

    # Linked files count (PDF, DOCX, etc.)
    main_cursor.execute("SELECT COUNT(*) FROM linked_files")
    stats["linked_files_count"] = main_cursor.fetchone()[0]

    # Headline count
    main_cursor.execute("SELECT COUNT(*) FROM headlines")
    stats["headlines_count"] = main_cursor.fetchone()[0]

    # Link count
    main_cursor.execute("SELECT COUNT(*) FROM links")
    stats["links_count"] = main_cursor.fetchone()[0]

    # FTS content count
    main_cursor.execute("SELECT COUNT(*) FROM fts_content")
    stats["fts_entries_count"] = main_cursor.fetchone()[0]

    # Recent files (last 5)
    main_cursor.execute("""
        SELECT filename, indexed_at
        FROM files
        ORDER BY indexed_at DESC
        LIMIT 5
    """)
    stats["recent_files"] = [
        {"filename": row[0], "indexed_at": row[1]}
        for row in main_cursor.fetchall()
    ]

    # Semantic DB stats
    semantic_cursor = db.semantic_conn.cursor()

    # Chunk count
    semantic_cursor.execute("SELECT COUNT(*) FROM chunks")
    stats["chunks_count"] = semantic_cursor.fetchone()[0]

    # Embedding count
    semantic_cursor.execute("SELECT COUNT(*) FROM embeddings")
    stats["embeddings_count"] = semantic_cursor.fetchone()[0]

    # Image DB stats
    image_cursor = db.image_conn.cursor()

    # Image count
    image_cursor.execute("SELECT COUNT(*) FROM images")
    stats["images_count"] = image_cursor.fetchone()[0]

    # Image embedding count
    image_cursor.execute("SELECT COUNT(*) FROM image_embeddings")
    stats["image_embeddings_count"] = image_cursor.fetchone()[0]

    # Database file sizes and locations
    # Main DB
    stats["main_db_path"] = str(settings.db_path)
    if os.path.exists(settings.db_path):
        stats["main_db_size_bytes"] = os.path.getsize(settings.db_path)
        stats["main_db_size_mb"] = round(stats["main_db_size_bytes"] / (1024 * 1024), 2)
    else:
        stats["main_db_size_bytes"] = 0
        stats["main_db_size_mb"] = 0

    # Semantic DB
    stats["semantic_db_path"] = str(settings.semantic_db_path)
    if os.path.exists(settings.semantic_db_path):
        stats["semantic_db_size_bytes"] = os.path.getsize(settings.semantic_db_path)
        stats["semantic_db_size_mb"] = round(stats["semantic_db_size_bytes"] / (1024 * 1024), 2)
    else:
        stats["semantic_db_size_bytes"] = 0
        stats["semantic_db_size_mb"] = 0

    # Image DB
    stats["image_db_path"] = str(settings.image_db_path)
    if os.path.exists(settings.image_db_path):
        stats["image_db_size_bytes"] = os.path.getsize(settings.image_db_path)
        stats["image_db_size_mb"] = round(stats["image_db_size_bytes"] / (1024 * 1024), 2)
    else:
        stats["image_db_size_bytes"] = 0
        stats["image_db_size_mb"] = 0

    # Total size
    stats["total_db_size_bytes"] = (stats["main_db_size_bytes"] +
                                    stats["semantic_db_size_bytes"] +
                                    stats["image_db_size_bytes"])
    stats["total_db_size_mb"] = round(stats["total_db_size_bytes"] / (1024 * 1024), 2)

    # Legacy fields for backward compatibility
    stats["db_path"] = stats["main_db_path"]
    stats["db_size_bytes"] = stats["total_db_size_bytes"]
    stats["db_size_mb"] = stats["total_db_size_mb"]

    return stats

@router.get("/files", response_model=Dict[str, Any])
async def get_files():
    """Get all files in the database."""
    cursor = db.main_conn.cursor()

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

@router.post("/optimize", response_model=Dict[str, Any])
async def optimize_database():
    """Optimize database by running ANALYZE to update index statistics.

    This improves query performance, especially for vector searches.
    Should be run after bulk indexing operations or if queries seem slow.
    """
    try:
        db.optimize()
        return {
            "status": "success",
            "message": "Database optimized successfully"
        }
    except Exception as e:
        logger.error(f"Error optimizing database: {str(e)}")
        raise HTTPException(status_code=500, detail=f"Failed to optimize database: {str(e)}")

@router.delete("/clear-database", response_model=Dict[str, Any])
async def clear_database():
    """Clear all three databases by removing their files.

    WARNING: This is destructive and cannot be undone!
    All indexed data will be permanently deleted.
    """
    global db

    try:
        # Close all database connections first
        db.close()
        logger.info("Closed all database connections")

        # Delete all three database files
        deleted_files = []
        for db_path, db_name in [
            (settings.db_path, "main"),
            (settings.semantic_db_path, "semantic"),
            (settings.image_db_path, "image")
        ]:
            if db_path.exists():
                os.remove(db_path)
                logger.info(f"Deleted {db_name} database file: {db_path}")
                deleted_files.append(str(db_path))

        # Reinitialize all databases with fresh connections
        db = Database(settings.db_path, settings.semantic_db_path, settings.image_db_path)
        logger.info("Reinitialized all empty databases")

        return {
            "status": "success",
            "message": "All databases cleared successfully",
            "deleted_files": deleted_files
        }

    except Exception as e:
        logger.error(f"Error clearing databases: {str(e)}")
        raise HTTPException(status_code=500, detail=f"Failed to clear databases: {str(e)}")
