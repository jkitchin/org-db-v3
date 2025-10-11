"""Search API endpoints."""
from fastapi import APIRouter, HTTPException
import numpy as np
from typing import List, Tuple

from org_db_server.models.schemas import (
    SemanticSearchRequest, SemanticSearchResponse, SearchResult,
    FulltextSearchRequest, FulltextSearchResponse, FulltextSearchResult,
    ImageSearchRequest, ImageSearchResponse, ImageSearchResult
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
        cursor.execute("""
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
            WHERE e.embedding_model = ?
        """, (model_name,))

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
    """Perform full-text search using FTS5."""
    try:
        cursor = db.conn.cursor()

        # Query FTS5 table
        cursor.execute(
            f"SELECT filename, title, content, tags FROM fts_content WHERE fts_content MATCH ? LIMIT ?",
            (request.query, request.limit)
        )

        rows = cursor.fetchall()

        # Convert to result objects
        results = [
            FulltextSearchResult(
                filename=row[0],
                title=row[1],
                content=row[2],
                tags=row[3] or ""
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
