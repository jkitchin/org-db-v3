"""Indexing API endpoints."""
from fastapi import APIRouter, HTTPException
from datetime import datetime
from typing import List, Dict, Any
import psutil
import os

from org_db_server.models.schemas import IndexFileRequest, IndexFileResponse
from org_db_server.services.database import Database
from org_db_server.services.chunking import chunk_text
from org_db_server.services.embeddings import get_embedding_service
from org_db_server.services.clip_service import get_clip_service
from org_db_server.services.docling_service import get_docling_service
from org_db_server.config import settings
from pathlib import Path
import logging

logger = logging.getLogger(__name__)

def log_memory_usage(context: str = ""):
    """Log current memory usage for debugging."""
    try:
        process = psutil.Process()
        mem_info = process.memory_info()
        mem_mb = mem_info.rss / 1024 / 1024
        msg = f"[MEMORY{(' ' + context) if context else ''}] RSS: {mem_mb:.1f} MB, VMS: {mem_info.vms / 1024 / 1024:.1f} MB"
        logger.info(msg)

        # Also write to dedicated memory log file for debugging
        with open("/tmp/org-db-memory.log", "a") as f:
            from datetime import datetime
            timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S.%f")[:-3]
            f.write(f"{timestamp} - PID {os.getpid()} - {msg}\n")
    except Exception as e:
        logger.warning(f"Could not get memory info: {e}")

router = APIRouter(prefix="/api", tags=["indexing"])

# Global database instance (will be improved later with dependency injection)
db = Database(settings.db_path)

@router.get("/files")
async def get_files() -> Dict[str, Any]:
    """Get list of all files in the database."""
    try:
        cursor = db.conn.cursor()
        cursor.execute("SELECT filename, indexed_at FROM files ORDER BY indexed_at DESC")
        rows = cursor.fetchall()

        files = [
            {"filename": row[0], "indexed_at": row[1]}
            for row in rows
        ]

        return {"files": files}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

@router.delete("/file")
async def delete_file(filename: str) -> Dict[str, Any]:
    """Delete a file and all its associated data from the database."""
    try:
        cursor = db.conn.cursor()

        # Get file ID
        cursor.execute("SELECT rowid FROM files WHERE filename = ?", (filename,))
        row = cursor.fetchone()

        if not row:
            raise HTTPException(status_code=404, detail=f"File not found: {filename}")

        file_id = row[0]

        # Delete the file (CASCADE will handle related data)
        cursor.execute("DELETE FROM files WHERE rowid = ?", (file_id,))
        db.conn.commit()

        return {
            "status": "deleted",
            "filename": filename,
            "message": f"Successfully deleted {filename} and all associated data"
        }
    except HTTPException:
        raise
    except Exception as e:
        db.conn.rollback()
        raise HTTPException(status_code=500, detail=str(e))

@router.post("/file", response_model=IndexFileResponse)
async def index_file(request: IndexFileRequest):
    """Index an org file."""
    try:
        log_memory_usage("at start of indexing")

        # Debug: Log what we received
        logger.info(f"Indexing file: {request.filename}")
        logger.info(f"  Headlines: {len(request.headlines)}")
        logger.info(f"  Links: {len(request.links)}")
        logger.info(f"  Images: {len(request.images)}")
        logger.info(f"  Linked files: {len(request.linked_files)}")

        # Get or create file entry
        file_id = db.get_or_create_file_id(
            request.filename,
            request.md5,
            request.file_size
        )

        cursor = db.conn.cursor()

        # Delete existing data for this file (we'll re-index everything)
        cursor.execute("DELETE FROM headlines WHERE filename_id = ?", (file_id,))
        cursor.execute("DELETE FROM links WHERE filename_id = ?", (file_id,))
        cursor.execute("DELETE FROM file_keywords WHERE filename_id = ?", (file_id,))
        cursor.execute("DELETE FROM src_blocks WHERE filename_id = ?", (file_id,))

        # Insert headlines
        for hl in request.headlines:
            cursor.execute(
                """INSERT INTO headlines (filename_id, title, level, todo_keyword, todo_type,
                   archivedp, commentedp, begin, end, tags, priority, scheduled, deadline)
                   VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)""",
                (file_id, hl.title, hl.level, hl.todo_keyword, hl.todo_type,
                 hl.archivedp, hl.commentedp, hl.begin, hl.end, hl.tags,
                 hl.priority, hl.scheduled, hl.deadline)
            )
            headline_id = cursor.lastrowid

            # Insert properties if any
            if hl.properties:
                for prop_key, prop_value in hl.properties.items():
                    # Get or create property
                    cursor.execute("SELECT rowid FROM properties WHERE property = ?", (prop_key,))
                    prop_row = cursor.fetchone()

                    if prop_row:
                        property_id = prop_row[0]
                    else:
                        cursor.execute("INSERT INTO properties (property) VALUES (?)", (prop_key,))
                        property_id = cursor.lastrowid

                    # Insert headline property
                    cursor.execute(
                        "INSERT INTO headline_properties (headline_id, property_id, value) VALUES (?, ?, ?)",
                        (headline_id, property_id, prop_value)
                    )

        # Insert links
        for link in request.links:
            cursor.execute(
                """INSERT INTO links (filename_id, type, path, raw_link, description, search_option, begin)
                   VALUES (?, ?, ?, ?, ?, ?, ?)""",
                (file_id, link.type, link.path, link.raw_link, link.description, link.search_option, link.begin)
            )

        # Insert keywords
        for kw in request.keywords:
            # Get or create keyword
            cursor.execute("SELECT rowid FROM keywords WHERE keyword = ?", (kw.key,))
            kw_row = cursor.fetchone()

            if kw_row:
                keyword_id = kw_row[0]
            else:
                cursor.execute("INSERT INTO keywords (keyword) VALUES (?)", (kw.key,))
                keyword_id = cursor.lastrowid

            # Insert file keyword
            cursor.execute(
                "INSERT INTO file_keywords (filename_id, keyword_id, value, begin) VALUES (?, ?, ?, ?)",
                (file_id, keyword_id, kw.value, kw.begin)
            )

        # Insert src blocks
        for src in request.src_blocks:
            cursor.execute(
                "INSERT INTO src_blocks (filename_id, language, contents, begin) VALUES (?, ?, ?, ?)",
                (file_id, src.language, src.contents, src.begin)
            )

        # Generate chunks from full file content for semantic search
        if request.content:
            log_memory_usage("before chunking org content")

            # Chunk the full content with proper line tracking
            all_chunks = chunk_text(request.content, method="paragraph")

            # Generate embeddings
            if all_chunks:
                log_memory_usage(f"before embeddings for {len(all_chunks)} org chunks")
                embedding_service = get_embedding_service()
                chunk_texts = [c["text"] for c in all_chunks]
                embeddings = embedding_service.generate_embeddings(chunk_texts)

                log_memory_usage(f"after embeddings, before storing {len(all_chunks)} org chunks")
                # Store chunks and embeddings
                db.store_chunks(file_id, all_chunks, embeddings, embedding_service.model_name)
                log_memory_usage("after storing org chunks")

        # Populate FTS5 table with entire file content
        db.populate_fts(file_id, request.filename, request.content or "")

        # Process images with CLIP if any
        print(f"DEBUG: Received {len(request.images)} images from client")
        if request.images:
            images_with_embeddings = []
            images_data = []

            for img in request.images:
                print(f"DEBUG: Processing image: {img.path}")
                # Resolve image path relative to org file
                org_dir = Path(request.filename).parent
                img_path = org_dir / img.path
                print(f"DEBUG: Resolved path: {img_path}, exists={img_path.exists()}")

                if img_path.exists() and img_path.is_file():
                    try:
                        images_data.append({"path": str(img_path), "begin": img.begin})
                        print(f"DEBUG: Added image to process: {img_path}")
                    except Exception as e:
                        print(f"Error preparing image {img_path}: {e}")
                else:
                    print(f"DEBUG: Image file not found or not a file: {img_path}")

            # Generate CLIP embeddings for valid images
            if images_data:
                print(f"DEBUG: Generating CLIP embeddings for {len(images_data)} images")
                try:
                    clip_service = get_clip_service()
                    image_paths = [img["path"] for img in images_data]
                    embeddings = clip_service.generate_image_embeddings(image_paths)
                    print(f"DEBUG: Generated {len(embeddings)} embeddings")

                    # Store images and embeddings
                    db.store_images(file_id, images_data, embeddings, clip_service.model_name)
                    print(f"DEBUG: Successfully stored images and embeddings")
                except Exception as e:
                    print(f"Error generating CLIP embeddings: {e}")
                    import traceback
                    traceback.print_exc()
                    # Continue without image embeddings
            else:
                print(f"DEBUG: No valid images to process after path resolution")

        # Process linked files (PDF, DOCX, etc.)
        linked_files_indexed = 0

        if request.linked_files and not settings.enable_linked_files:
            logger.info(f"Skipping {len(request.linked_files)} linked files (feature disabled)")

        if request.linked_files and settings.enable_linked_files:
            log_memory_usage(f"before processing {len(request.linked_files)} linked files")
            logger.info(f"Processing {len(request.linked_files)} linked files for {request.filename}")
            docling = get_docling_service()

            for idx, linked_file in enumerate(request.linked_files, 1):
                try:
                    file_path = linked_file.file_path
                    org_link_line = linked_file.org_link_line

                    log_memory_usage(f"before converting linked file {idx}/{len(request.linked_files)}: {Path(file_path).name}")
                    logger.info(f"Converting linked file: {file_path}")

                    # Convert file to markdown
                    # PDFs use lightweight pymupdf4llm, others use docling in subprocess
                    conversion = docling.convert_to_markdown(file_path)
                    log_memory_usage(f"after converting {Path(file_path).name}")

                    # Determine file type
                    file_type = Path(file_path).suffix.lstrip('.')

                    # Create/update linked file entry
                    linked_file_id = db.get_or_create_linked_file(
                        org_file_id=file_id,
                        org_link_line=org_link_line,
                        file_path=file_path,
                        file_type=file_type,
                        file_size=conversion.get('file_size', 0),
                        md5=conversion.get('md5', ''),
                        conversion_status=conversion['status'],
                        conversion_error=conversion.get('error')
                    )

                    # If conversion succeeded, chunk and index
                    if conversion['status'] == 'success':
                        markdown = conversion['markdown']

                        log_memory_usage(f"before chunking {Path(file_path).name}")
                        # Chunk the markdown
                        chunk_results = chunk_text(markdown, method="paragraph")

                        if chunk_results:
                            log_memory_usage(f"before embeddings for {len(chunk_results)} linked file chunks from {Path(file_path).name}")
                            # Generate embeddings
                            texts = [c['text'] for c in chunk_results]
                            embeddings = embedding_service.generate_embeddings(texts)

                            log_memory_usage(f"after embeddings, before storing linked file chunks from {Path(file_path).name}")
                            # Store chunks
                            db.store_linked_file_chunks(
                                org_file_id=file_id,
                                org_link_line=org_link_line,
                                linked_file_id=linked_file_id,
                                chunks=chunk_results,
                                embeddings=embeddings,
                                model_name=embedding_service.model_name
                            )

                            linked_files_indexed += 1
                            log_memory_usage(f"after indexing linked file {idx}/{len(request.linked_files)}: {Path(file_path).name}")
                            logger.info(f"Indexed linked file: {file_path} ({len(chunk_results)} chunks)")
                        else:
                            logger.warning(f"No chunks generated for {file_path}")
                    else:
                        logger.warning(f"Conversion failed for {file_path}: {conversion.get('error')}")

                except Exception as e:
                    logger.error(f"Error processing linked file {linked_file.file_path}: {e}", exc_info=True)
                    log_memory_usage(f"after error processing {Path(linked_file.file_path).name}")
                    # Continue with other files

            # Force garbage collection after processing all linked files
            import gc
            gc.collect()
            log_memory_usage("after gc.collect() for linked files")

        log_memory_usage("before final commit")
        db.conn.commit()
        log_memory_usage("after commit, indexing complete")

        # Final garbage collection to free memory for next request
        import gc
        gc.collect()

        return IndexFileResponse(
            file_id=file_id,
            status="indexed",
            headlines_count=len(request.headlines),
            links_count=len(request.links),
            linked_files_count=linked_files_indexed
        )

    except Exception as e:
        db.conn.rollback()
        raise HTTPException(status_code=500, detail=str(e))
