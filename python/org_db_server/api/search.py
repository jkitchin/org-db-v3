"""Search API endpoints."""
from fastapi import APIRouter, HTTPException
import numpy as np
from typing import List, Tuple

from org_db_server.models.schemas import (
    SemanticSearchRequest, SemanticSearchResponse, SearchResult,
    FulltextSearchRequest, FulltextSearchResponse, FulltextSearchResult,
    ImageSearchRequest, ImageSearchResponse, ImageSearchResult,
    HeadlineSearchRequest, HeadlineSearchResponse, HeadlineSearchResult
)
from org_db_server.services.database import Database
from org_db_server.services.embeddings import get_embedding_service
from org_db_server.services.clip_service import get_clip_service
from org_db_server.config import settings

router = APIRouter(prefix="/api/search", tags=["search"])

# Global database instance
db = Database(settings.db_path)

@router.post("/semantic", response_model=SemanticSearchResponse)
async def semantic_search(request: SemanticSearchRequest):
    """Perform semantic search using embeddings."""
    try:
        # Get embedding service
        model_name = request.model or "all-MiniLM-L6-v2"
        embedding_service = get_embedding_service(model_name)

        # Generate query embedding
        query_embedding = embedding_service.generate_embedding(request.query)

        # Fetch all stored embeddings from database
        cursor = db.conn.cursor()

        # Build query with optional filters
        base_query = """
            SELECT
                e.rowid as chunk_id,
                e.embedding_vector,
                c.chunk_text,
                c.chunk_type,
                c.begin_line,
                c.end_line,
                f.filename
            FROM embeddings e
            JOIN chunks c ON e.chunk_id = c.rowid
            JOIN files f ON c.filename_id = f.rowid
        """

        params = [model_name]
        where_clauses = ["e.embedding_model = ?"]

        # Add filename pattern filter if provided
        if request.filename_pattern:
            where_clauses.append("f.filename LIKE ?")
            params.append(request.filename_pattern)

        # Add keyword filter if provided
        if request.keyword:
            base_query += """
                JOIN file_keywords fk ON f.rowid = fk.filename_id
                JOIN keywords k ON fk.keyword_id = k.rowid
            """
            where_clauses.append("k.keyword = ?")
            params.append(request.keyword)

        # Combine WHERE clauses
        base_query += " WHERE " + " AND ".join(where_clauses)

        cursor.execute(base_query, params)
        rows = cursor.fetchall()

        if not rows:
            return SemanticSearchResponse(
                results=[],
                query=request.query,
                model_used=model_name
            )

        # Calculate similarity scores
        results_with_scores: List[Tuple[float, dict]] = []

        for row in rows:
            # Convert bytes back to numpy array
            embedding_bytes = row[1]
            stored_embedding = np.frombuffer(embedding_bytes, dtype=np.float32)

            # Calculate cosine similarity
            similarity = embedding_service.similarity(query_embedding, stored_embedding)

            result_data = {
                "chunk_id": row[0],
                "chunk_text": row[2],
                "chunk_type": row[3],
                "begin_line": row[4],
                "end_line": row[5],
                "filename": row[6],
                "similarity_score": float(similarity)
            }

            results_with_scores.append((similarity, result_data))

        # Sort by similarity (highest first) and take top N
        results_with_scores.sort(key=lambda x: x[0], reverse=True)
        top_results = results_with_scores[:request.limit]

        # Convert to SearchResult objects
        search_results = [
            SearchResult(**result_data)
            for _, result_data in top_results
        ]

        return SemanticSearchResponse(
            results=search_results,
            query=request.query,
            model_used=model_name
        )

    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

@router.post("/fulltext", response_model=FulltextSearchResponse)
async def fulltext_search(request: FulltextSearchRequest):
    """Perform full-text search using FTS5 with snippets and relevance ranking."""
    try:
        cursor = db.conn.cursor()

        # Build query with optional filters
        # Query FTS5 table with snippet() for highlighted context and bm25() for relevance
        # snippet() parameters: column, start_tag, end_tag, ellipsis, token_count
        # We use '>>>' and '<<<' as markers that can be removed/highlighted in Elisp
        base_query = """
            SELECT
                fts.filename,
                fts.title,
                fts.content,
                fts.tags,
                snippet(fts_content, 2, '>>>', '<<<', '...', 15) as snippet,
                bm25(fts_content) as rank
            FROM fts_content fts
        """

        params = [request.query]
        where_clauses = ["fts_content MATCH ?"]

        # Add filename pattern filter if provided
        if request.filename_pattern:
            # Need to join with files table for filename pattern
            base_query = """
                SELECT
                    fts.filename,
                    fts.title,
                    fts.content,
                    fts.tags,
                    snippet(fts_content, 2, '>>>', '<<<', '...', 15) as snippet,
                    bm25(fts_content) as rank
                FROM fts_content fts
                JOIN files f ON fts.filename = f.filename
            """
            where_clauses.append("f.filename LIKE ?")
            params.append(request.filename_pattern)

        # Add keyword filter if provided
        if request.keyword:
            if "JOIN files f" not in base_query:
                base_query = base_query.replace(
                    "FROM fts_content fts",
                    "FROM fts_content fts JOIN files f ON fts.filename = f.filename"
                )
            base_query = base_query.replace(
                "JOIN files f ON fts.filename = f.filename",
                """JOIN files f ON fts.filename = f.filename
                JOIN file_keywords fk ON f.rowid = fk.filename_id
                JOIN keywords k ON fk.keyword_id = k.rowid"""
            )
            where_clauses.append("k.keyword = ?")
            params.append(request.keyword)

        # Combine WHERE clauses
        base_query += " WHERE " + " AND ".join(where_clauses)
        base_query += " ORDER BY rank LIMIT ?"
        params.append(request.limit)

        cursor.execute(base_query, params)
        rows = cursor.fetchall()

        # Convert to result objects
        results = [
            FulltextSearchResult(
                filename=row[0],
                title=row[1],
                content=row[2],
                tags=row[3] or "",
                snippet=row[4],
                rank=float(row[5])
            )
            for row in rows
        ]

        return FulltextSearchResponse(
            results=results,
            query=request.query
        )

    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

@router.post("/images", response_model=ImageSearchResponse)
async def image_search(request: ImageSearchRequest):
    """Perform image search using CLIP embeddings."""
    try:
        # Get CLIP service
        clip_service = get_clip_service()

        # Generate text embedding for the query
        query_embedding = clip_service.generate_text_embedding(request.query)

        # Fetch all stored image embeddings from database
        cursor = db.conn.cursor()
        cursor.execute("""
            SELECT
                ie.image_id,
                ie.embedding_vector,
                i.image_path,
                f.filename
            FROM image_embeddings ie
            JOIN images i ON ie.image_id = i.rowid
            JOIN files f ON i.filename_id = f.rowid
            WHERE ie.clip_model = ?
        """, (clip_service.model_name,))

        rows = cursor.fetchall()

        if not rows:
            return ImageSearchResponse(
                results=[],
                query=request.query,
                model_used=clip_service.model_name
            )

        # Calculate similarity scores
        results_with_scores: List[Tuple[float, dict]] = []

        for row in rows:
            # Convert bytes back to numpy array
            embedding_bytes = row[1]
            stored_embedding = np.frombuffer(embedding_bytes, dtype=np.float32)

            # Calculate cosine similarity
            similarity = clip_service.similarity(query_embedding, stored_embedding)

            result_data = {
                "image_path": row[2],
                "filename": row[3],
                "similarity_score": float(similarity)
            }

            results_with_scores.append((similarity, result_data))

        # Sort by similarity (highest first) and take top N
        results_with_scores.sort(key=lambda x: x[0], reverse=True)
        top_results = results_with_scores[:request.limit]

        # Convert to ImageSearchResult objects
        search_results = [
            ImageSearchResult(**result_data)
            for _, result_data in top_results
        ]

        return ImageSearchResponse(
            results=search_results,
            query=request.query,
            model_used=clip_service.model_name
        )

    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

@router.post("/headlines", response_model=HeadlineSearchResponse)
async def headline_search(request: HeadlineSearchRequest):
    """Search or list all headlines."""
    try:
        cursor = db.conn.cursor()

        if request.query:
            # Search headlines by title using LIKE
            cursor.execute("""
                SELECT h.title, f.filename, h.begin, h.level, h.tags, h.todo_keyword
                FROM headlines h
                JOIN files f ON h.filename_id = f.rowid
                WHERE h.title LIKE ?
                ORDER BY f.filename, h.begin
                LIMIT ?
            """, (f"%{request.query}%", request.limit))
        else:
            # Return all headlines
            cursor.execute("""
                SELECT h.title, f.filename, h.begin, h.level, h.tags, h.todo_keyword
                FROM headlines h
                JOIN files f ON h.filename_id = f.rowid
                ORDER BY f.filename, h.begin
                LIMIT ?
            """, (request.limit,))

        rows = cursor.fetchall()

        # Convert to result objects
        results = [
            HeadlineSearchResult(
                title=row[0],
                filename=row[1],
                begin=row[2],
                level=row[3],
                tags=row[4],
                todo_keyword=row[5]
            )
            for row in rows
        ]

        return HeadlineSearchResponse(
            results=results,
            query=request.query
        )

    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))
