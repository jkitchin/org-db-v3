"""API endpoints for linked file indexing."""
import logging
from typing import List
from fastapi import APIRouter, HTTPException
from pydantic import BaseModel, Field

from org_db_server.services.document_converter import get_document_converter
from org_db_server.services.embeddings import get_embedding_service
from org_db_server.services.database import Database
from org_db_server.services.chunking import chunk_text
from org_db_server.config import settings

logger = logging.getLogger(__name__)

router = APIRouter(prefix="/api/linked-files", tags=["linked-files"])

# Global database instance
db = Database(settings.db_path, settings.semantic_db_path, settings.image_db_path)


class LinkedFileRequest(BaseModel):
    """Request to index a linked file."""
    org_filename: str = Field(..., description="Org file containing the link")
    org_link_line: int = Field(..., description="Line number in org file where link exists")
    file_path: str = Field(..., description="Absolute path to the linked file")
    max_file_size: int = Field(default=52428800, description="Maximum file size in bytes (default 50MB)")
    embedding_model: str = Field(default="all-MiniLM-L6-v2", description="Embedding model to use")


class LinkedFileResponse(BaseModel):
    """Response from indexing a linked file."""
    status: str = Field(..., description="Status: success, error, skipped")
    linked_file_id: int = Field(default=None, description="Database ID of the linked file entry")
    chunk_count: int = Field(default=0, description="Number of chunks created")
    error: str = Field(default=None, description="Error message if failed")
    file_size: int = Field(default=0, description="Size of the file in bytes")
    file_type: str = Field(default=None, description="File extension/type")
    md5: str = Field(default=None, description="MD5 hash of the file")


@router.post("/index", response_model=LinkedFileResponse)
async def index_linked_file(request: LinkedFileRequest):
    """Index a file linked from an org file.

    This endpoint:
    1. Converts the file to markdown using document converter
    2. Chunks the markdown
    3. Generates embeddings
    4. Stores chunks in database (pointing to org file location)

    All chunks from the linked file point to the org file location where
    the link exists, not to the linked file itself. This preserves context.
    """
    try:
        # Get services
        converter = get_document_converter()
        embedding_service = get_embedding_service()

        # Get org file ID from main DB
        cursor = db.main_conn.cursor()
        cursor.execute("SELECT rowid FROM files WHERE filename = ?", (request.org_filename,))
        row = cursor.fetchone()

        if not row:
            raise HTTPException(
                status_code=404,
                detail=f"Org file not found in database: {request.org_filename}"
            )

        org_file_id = row[0]

        # Convert file to markdown
        logger.info(f"Converting linked file: {request.file_path}")
        conversion_result = converter.convert_to_markdown(
            request.file_path,
            max_file_size=request.max_file_size
        )

        # Determine file type
        from pathlib import Path
        file_type = Path(request.file_path).suffix.lstrip('.')

        # If conversion failed or was skipped, record it and return
        if conversion_result['status'] != 'success':
            linked_file_id = db.get_or_create_linked_file(
                org_file_id=org_file_id,
                org_link_line=request.org_link_line,
                file_path=request.file_path,
                file_type=file_type,
                file_size=conversion_result.get('file_size', 0),
                md5=conversion_result.get('md5') or '',
                conversion_status=conversion_result['status'],
                conversion_error=conversion_result.get('error')
            )
            db.main_conn.commit()

            return LinkedFileResponse(
                status=conversion_result['status'],
                linked_file_id=linked_file_id,
                chunk_count=0,
                error=conversion_result.get('error'),
                file_size=conversion_result.get('file_size', 0),
                file_type=file_type,
                md5=conversion_result.get('md5')
            )

        # Chunk the markdown
        markdown_text = conversion_result['markdown']
        chunk_results = chunk_text(markdown_text, method="paragraph")

        # Prepare chunk data (already in dict format from chunk_text)
        chunk_dicts = chunk_results

        # Generate embeddings
        texts = [c['text'] for c in chunk_results]
        embeddings = embedding_service.generate_embeddings(texts)

        # Create/update linked file entry
        linked_file_id = db.get_or_create_linked_file(
            org_file_id=org_file_id,
            org_link_line=request.org_link_line,
            file_path=request.file_path,
            file_type=file_type,
            file_size=conversion_result['file_size'],
            md5=conversion_result['md5'],
            conversion_status='success',
            conversion_error=None
        )
        db.main_conn.commit()

        # Store chunks and embeddings (semantic DB uses filename not file_id)
        db.store_linked_file_chunks(
            org_filename=request.org_filename,
            org_link_line=request.org_link_line,
            linked_file_path=request.file_path,
            chunks=chunk_dicts,
            embeddings=embeddings,
            model_name=embedding_service.model_name
        )

        logger.info(f"Successfully indexed linked file: {request.file_path} ({len(chunk_results)} chunks)")

        return LinkedFileResponse(
            status="success",
            linked_file_id=linked_file_id,
            chunk_count=len(chunk_results),
            file_size=conversion_result['file_size'],
            file_type=file_type,
            md5=conversion_result['md5']
        )

    except Exception as e:
        logger.error(f"Error indexing linked file {request.file_path}: {e}", exc_info=True)
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/org-file/{org_filename}", response_model=List[dict])
async def get_linked_files_for_org_file(org_filename: str):
    """Get all linked files for an org file."""
    try:
        # Get org file ID from main DB
        cursor = db.main_conn.cursor()
        cursor.execute("SELECT rowid FROM files WHERE filename = ?", (org_filename,))
        row = cursor.fetchone()

        if not row:
            raise HTTPException(
                status_code=404,
                detail=f"Org file not found in database: {org_filename}"
            )

        org_file_id = row[0]

        # Get linked files
        linked_files = db.get_linked_files_for_org_file(org_file_id)

        return linked_files

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error getting linked files for {org_filename}: {e}", exc_info=True)
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/all", response_model=dict)
async def list_all_linked_files():
    """List all linked files in the database."""
    try:
        # Query main DB for linked files metadata
        cursor = db.main_conn.cursor()
        cursor.execute("""
            SELECT
                lf.rowid,
                lf.file_path,
                lf.file_type,
                lf.file_size,
                lf.conversion_status,
                lf.indexed_at,
                f.filename as org_filename,
                lf.org_link_line
            FROM linked_files lf
            JOIN files f ON lf.org_file_id = f.rowid
            ORDER BY lf.indexed_at DESC
        """)

        rows = cursor.fetchall()

        # Query semantic DB for chunk counts per linked file
        semantic_cursor = db.semantic_conn.cursor()
        semantic_cursor.execute("""
            SELECT linked_file_path, COUNT(*) as chunk_count
            FROM chunks
            WHERE linked_file_path IS NOT NULL
            GROUP BY linked_file_path
        """)
        chunk_counts = {row[0]: row[1] for row in semantic_cursor.fetchall()}

        linked_files = []
        for row in rows:
            file_path = row[1]
            linked_files.append({
                "id": row[0],
                "file_path": file_path,
                "file_type": row[2],
                "file_size": row[3],
                "conversion_status": row[4],
                "indexed_at": row[5],
                "org_filename": row[6],
                "org_link_line": row[7],
                "chunk_count": chunk_counts.get(file_path, 0)
            })

        return {
            "linked_files": linked_files,
            "count": len(linked_files)
        }

    except Exception as e:
        logger.error(f"Error listing linked files: {e}", exc_info=True)
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/{linked_file_id}", response_model=dict)
async def get_linked_file_info(linked_file_id: int):
    """Get information about a specific linked file."""
    try:
        info = db.get_linked_file_info(linked_file_id)

        if not info:
            raise HTTPException(
                status_code=404,
                detail=f"Linked file not found: {linked_file_id}"
            )

        return info

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error getting linked file info for {linked_file_id}: {e}", exc_info=True)
        raise HTTPException(status_code=500, detail=str(e))
