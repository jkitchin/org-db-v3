"""Search API endpoints."""
from fastapi import APIRouter, HTTPException
import numpy as np
import logging
from typing import List, Tuple

logger = logging.getLogger(__name__)

from org_db_server.models.schemas import (
    SemanticSearchRequest, SemanticSearchResponse, SearchResult,
    FulltextSearchRequest, FulltextSearchResponse, FulltextSearchResult,
    ImageSearchRequest, ImageSearchResponse, ImageSearchResult,
    HeadlineSearchRequest, HeadlineSearchResponse, HeadlineSearchResult
)
from org_db_server.services.database import Database
from org_db_server.services.embeddings import get_embedding_service
from org_db_server.services.clip_service import get_clip_service
from org_db_server.services.reranker import get_reranker_service
from org_db_server.config import settings

router = APIRouter(prefix="/api/search", tags=["search"])

# Global database instance
db = Database(settings.db_path)

@router.post("/semantic", response_model=SemanticSearchResponse)
async def semantic_search(request: SemanticSearchRequest):
    """Perform semantic search using embeddings with fast vector_top_k()."""
    import time
    start_time = time.perf_counter()
    try:
        # Get embedding service
        t1 = time.perf_counter()
        model_name = request.model or "all-MiniLM-L6-v2"
        embedding_service = get_embedding_service(model_name)
        logger.debug(f"Get embedding service: {(time.perf_counter()-t1)*1000:.1f}ms")

        # Generate query embedding
        t1 = time.perf_counter()
        query_embedding = embedding_service.generate_embedding(request.query)
        logger.info(f"Generate embedding: {(time.perf_counter()-t1)*1000:.1f}ms")

        # Convert query embedding to bytes for libsql
        query_bytes = query_embedding.astype(np.float32).tobytes()

        # Use global database connection (now with correct path)
        cursor = db.conn.cursor()

        # Count total embeddings to decide between exact vs ANN search
        cursor.execute("SELECT COUNT(*) FROM embeddings WHERE embedding_model = ?", [model_name])
        total_embeddings = cursor.fetchone()[0]

        # Determine how many candidates to retrieve for reranking
        num_candidates = request.rerank_candidates if request.rerank else request.limit

        # Use exact search for small datasets (<5000) and ANN for large datasets
        # Exact search is more accurate and faster for small-to-medium datasets
        use_exact_search = total_embeddings < 5000

        # Build query with optional filters
        t1 = time.perf_counter()

        if use_exact_search:
            # Exact search: fetch all embeddings and calculate similarities in Python
            logger.debug(f"Using exact search for {total_embeddings} embeddings")

            base_query = """
                SELECT
                    e.chunk_id,
                    e.embedding_vector,
                    c.chunk_text,
                    c.chunk_type,
                    c.begin_line,
                    c.end_line,
                    f.filename,
                    lf.file_path as linked_file_path,
                    lf.file_type as linked_file_type
                FROM embeddings e
                JOIN chunks c ON e.chunk_id = c.rowid
                JOIN files f ON c.filename_id = f.rowid
                LEFT JOIN linked_files lf ON c.linked_file_id = lf.rowid
            """

            params = [model_name]
            where_clauses = ["e.embedding_model = ?"]

            if request.filename_pattern:
                where_clauses.append("f.filename LIKE ?")
                params.append(request.filename_pattern)

            if request.keyword:
                base_query += """
                    JOIN file_keywords fk ON f.rowid = fk.filename_id
                    JOIN keywords k ON fk.keyword_id = k.rowid
                """
                where_clauses.append("k.keyword = ?")
                params.append(request.keyword)

            base_query += " WHERE " + " AND ".join(where_clauses)
            cursor.execute(base_query, params)
            rows = cursor.fetchall()
            logger.info(f"Exact search query: {(time.perf_counter()-t1)*1000:.1f}ms")

            if not rows:
                return SemanticSearchResponse(
                    results=[],
                    query=request.query,
                    model_used=model_name
                )

            # Calculate similarities for all chunks
            t1 = time.perf_counter()
            results_with_scores = []
            for row in rows:
                chunk_id = row[0]
                embedding_bytes = row[1]
                chunk_text = row[2]
                chunk_type = row[3]
                begin_line = row[4]
                end_line = row[5]
                filename = row[6]
                linked_file_path = row[7]
                linked_file_type = row[8]

                stored_embedding = np.frombuffer(embedding_bytes, dtype=np.float32)
                similarity = float(embedding_service.similarity(query_embedding, stored_embedding))

                # Add file extension prefix for non-org files
                if linked_file_path and linked_file_type:
                    ext = linked_file_type.upper() if linked_file_type else linked_file_path.split('.')[-1].upper()
                    chunk_text = f"[{ext}] {chunk_text}"

                results_with_scores.append((similarity, SearchResult(
                    chunk_id=chunk_id,
                    chunk_text=chunk_text,
                    chunk_type=chunk_type,
                    begin_line=begin_line,
                    end_line=end_line,
                    filename=filename,
                    similarity_score=similarity,
                    linked_file_path=linked_file_path,
                    linked_file_type=linked_file_type
                )))

            # Sort by similarity (highest first) and take top N
            results_with_scores.sort(key=lambda x: x[0], reverse=True)
            search_results = [result for _, result in results_with_scores[:num_candidates]]
            logger.info(f"Similarity calculation: {(time.perf_counter()-t1)*1000:.1f}ms")

        else:
            # Use vector_top_k for larger datasets
            if request.filename_pattern or request.keyword:
                # With filters: join vector_top_k results with filtered files
                filter_query = """
                    SELECT f.rowid FROM files f
                    WHERE 1=1
                """
                filter_params = []

                if request.filename_pattern:
                    filter_query += " AND f.filename LIKE ?"
                    filter_params.append(request.filename_pattern)

                if request.keyword:
                    filter_query += """
                        AND EXISTS (
                            SELECT 1 FROM file_keywords fk
                            JOIN keywords k ON fk.keyword_id = k.rowid
                            WHERE fk.filename_id = f.rowid AND k.keyword = ?
                        )
                    """
                    filter_params.append(request.keyword)

                # Get matching file IDs
                cursor.execute(filter_query, filter_params)
                matching_file_ids = {row[0] for row in cursor.fetchall()}

                if not matching_file_ids:
                    return SemanticSearchResponse(
                        results=[],
                        query=request.query,
                        model_used=model_name
                    )

                # Get more candidates from vector search and filter
                cursor.execute("""
                    SELECT
                        vt.id as chunk_id,
                        c.chunk_text,
                        c.chunk_type,
                        c.begin_line,
                        c.end_line,
                        f.filename,
                        lf.file_path as linked_file_path,
                        lf.file_type as linked_file_type,
                        e.embedding_vector
                    FROM vector_top_k('idx_embeddings_vector', ?, ?) vt
                    JOIN embeddings e ON e.rowid = vt.id
                    JOIN chunks c ON c.rowid = e.chunk_id
                    JOIN files f ON c.filename_id = f.rowid
                    LEFT JOIN linked_files lf ON c.linked_file_id = lf.rowid
                    WHERE e.embedding_model = ? AND f.rowid IN ({})
                """.format(','.join('?' * len(matching_file_ids))),
                    [query_bytes, num_candidates * 3, model_name] + list(matching_file_ids)
                )
                rows = cursor.fetchall()[:num_candidates]
                logger.info(f"Vector search with filters: {(time.perf_counter()-t1)*1000:.1f}ms")
            else:
                # No filters: use vector_top_k directly
                cursor.execute("""
                    SELECT
                        vt.id as chunk_id,
                        c.chunk_text,
                        c.chunk_type,
                        c.begin_line,
                        c.end_line,
                        f.filename,
                        lf.file_path as linked_file_path,
                        lf.file_type as linked_file_type,
                        e.embedding_vector
                    FROM vector_top_k('idx_embeddings_vector', ?, ?) vt
                    JOIN embeddings e ON e.rowid = vt.id
                    JOIN chunks c ON c.rowid = e.chunk_id
                    JOIN files f ON c.filename_id = f.rowid
                    LEFT JOIN linked_files lf ON c.linked_file_id = lf.rowid
                    WHERE e.embedding_model = ?
                """, [query_bytes, num_candidates, model_name])
                rows = cursor.fetchall()
                logger.info(f"Vector search (no filters): {(time.perf_counter()-t1)*1000:.1f}ms")

            # Build results from vector_top_k output (common for both filtered and unfiltered)
            if not rows:
                return SemanticSearchResponse(
                    results=[],
                    query=request.query,
                    model_used=model_name
                )

            # vector_top_k returns results in order, but we need to calculate similarity
            search_results = []
            for row in rows:
                chunk_id = row[0]
                chunk_text = row[1]
                chunk_type = row[2]
                begin_line = row[3]
                end_line = row[4]
                filename = row[5]
                linked_file_path = row[6]
                linked_file_type = row[7]
                embedding_bytes = row[8]

                # Calculate cosine similarity
                stored_embedding = np.frombuffer(embedding_bytes, dtype=np.float32)
                similarity = float(embedding_service.similarity(query_embedding, stored_embedding))

                # Add file extension prefix for non-org files
                if linked_file_path and linked_file_type:
                    ext = linked_file_type.upper() if linked_file_type else linked_file_path.split('.')[-1].upper()
                    chunk_text = f"[{ext}] {chunk_text}"

                search_results.append(SearchResult(
                    chunk_id=chunk_id,
                    chunk_text=chunk_text,
                    chunk_type=chunk_type,
                    begin_line=begin_line,
                    end_line=end_line,
                    filename=filename,
                    similarity_score=similarity,
                    linked_file_path=linked_file_path,
                    linked_file_type=linked_file_type
                ))

        # Apply cross-encoder reranking if requested
        reranked = False
        if request.rerank and len(search_results) > 0:
            try:
                reranker = get_reranker_service()
                result_dicts = [r.model_dump() for r in search_results]
                reranked_dicts = reranker.rerank(
                    query=request.query,
                    results=result_dicts,
                    text_field="chunk_text",
                    score_field="similarity_score",
                    top_k=request.limit
                )
                search_results = [SearchResult(**d) for d in reranked_dicts]
                reranked = True
            except Exception as e:
                logger.warning(f"Reranking failed, using original results: {e}")
                search_results = search_results[:request.limit]
        else:
            # Just take top N results
            search_results = search_results[:request.limit]

        total_time = time.perf_counter() - start_time
        logger.info(f"Semantic search TOTAL: {total_time*1000:.1f}ms (query='{request.query}', results={len(search_results)})")

        return SemanticSearchResponse(
            results=search_results,
            query=request.query,
            model_used=model_name,
            reranked=reranked
        )

    except Exception as e:
        logger.error(f"Semantic search error: {e}", exc_info=True)
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
    """Perform image search using CLIP embeddings.

    Uses exact search for small datasets (<1000 images) and fast vector_top_k()
    for larger datasets. Exact search is more accurate for small datasets.
    """
    try:
        # Get CLIP service
        clip_service = get_clip_service()

        # Generate text embedding for the query
        query_embedding = clip_service.generate_text_embedding(request.query)

        cursor = db.conn.cursor()

        # Count total images to decide between exact vs ANN search
        cursor.execute("SELECT COUNT(*) FROM image_embeddings WHERE clip_model = ?",
                      [clip_service.model_name])
        total_images = cursor.fetchone()[0]

        # Use exact search for small datasets (more accurate)
        # Use ANN search for large datasets (much faster)
        use_exact_search = total_images < 1000

        if use_exact_search:
            # Exact search: fetch all embeddings and calculate similarities
            logger.debug(f"Using exact search for {total_images} images")

            # Build query with optional filters
            base_query = """
                SELECT
                    ie.rowid,
                    ie.embedding_vector,
                    i.image_path,
                    f.filename
                FROM image_embeddings ie
                JOIN images i ON ie.image_id = i.rowid
                JOIN files f ON i.filename_id = f.rowid
            """

            params = [clip_service.model_name]
            where_clauses = ["ie.clip_model = ?"]

            if request.filename_pattern:
                where_clauses.append("f.filename LIKE ?")
                params.append(request.filename_pattern)

            if request.keyword:
                base_query += """
                    JOIN file_keywords fk ON f.rowid = fk.filename_id
                    JOIN keywords k ON fk.keyword_id = k.rowid
                """
                where_clauses.append("k.keyword = ?")
                params.append(request.keyword)

            base_query += " WHERE " + " AND ".join(where_clauses)
            cursor.execute(base_query, params)
            rows = cursor.fetchall()

            if not rows:
                return ImageSearchResponse(
                    results=[],
                    query=request.query,
                    model_used=clip_service.model_name
                )

            # Calculate similarities for all images
            results_with_scores: List[Tuple[float, str, str]] = []
            for row in rows:
                embedding_bytes = row[1]
                image_path = row[2]
                filename = row[3]

                stored_embedding = np.frombuffer(embedding_bytes, dtype=np.float32)
                similarity = clip_service.similarity(query_embedding, stored_embedding)
                results_with_scores.append((float(similarity), image_path, filename))

            # Sort by similarity (highest first) and take top N
            results_with_scores.sort(key=lambda x: x[0], reverse=True)
            top_results = results_with_scores[:request.limit]

            search_results = [
                ImageSearchResult(
                    image_path=image_path,
                    filename=filename,
                    similarity_score=similarity
                )
                for similarity, image_path, filename in top_results
            ]

        else:
            # ANN search: use vector_top_k for large datasets
            logger.debug(f"Using ANN search for {total_images} images")

            # Convert query embedding to bytes for libsql
            query_bytes = query_embedding.astype(np.float32).tobytes()

            if request.filename_pattern or request.keyword:
                # With filters: filter files first
                filter_query = "SELECT f.rowid FROM files f WHERE 1=1"
                filter_params = []

                if request.filename_pattern:
                    filter_query += " AND f.filename LIKE ?"
                    filter_params.append(request.filename_pattern)

                if request.keyword:
                    filter_query += """
                        AND EXISTS (
                            SELECT 1 FROM file_keywords fk
                            JOIN keywords k ON fk.keyword_id = k.rowid
                            WHERE fk.filename_id = f.rowid AND k.keyword = ?
                        )
                    """
                    filter_params.append(request.keyword)

                cursor.execute(filter_query, filter_params)
                matching_file_ids = {row[0] for row in cursor.fetchall()}

                if not matching_file_ids:
                    return ImageSearchResponse(
                        results=[],
                        query=request.query,
                        model_used=clip_service.model_name
                    )

                # Get results from vector search filtered by file IDs
                cursor.execute("""
                    SELECT
                        i.image_path,
                        f.filename,
                        ie.embedding_vector
                    FROM vector_top_k('idx_image_embeddings_vector', ?, ?) vt
                    JOIN image_embeddings ie ON ie.rowid = vt.id
                    JOIN images i ON ie.image_id = i.rowid
                    JOIN files f ON i.filename_id = f.rowid
                    WHERE ie.clip_model = ? AND f.rowid IN ({})
                """.format(','.join('?' * len(matching_file_ids))),
                    [query_bytes, request.limit * 2, clip_service.model_name] + list(matching_file_ids)
                )
                rows = cursor.fetchall()[:request.limit]
            else:
                # No filters: use vector_top_k directly
                cursor.execute("""
                    SELECT
                        i.image_path,
                        f.filename,
                        ie.embedding_vector
                    FROM vector_top_k('idx_image_embeddings_vector', ?, ?) vt
                    JOIN image_embeddings ie ON ie.rowid = vt.id
                    JOIN images i ON ie.image_id = i.rowid
                    JOIN files f ON i.filename_id = f.rowid
                    WHERE ie.clip_model = ?
                """, [query_bytes, request.limit, clip_service.model_name])
                rows = cursor.fetchall()

            if not rows:
                return ImageSearchResponse(
                    results=[],
                    query=request.query,
                    model_used=clip_service.model_name
                )

            # Build results from vector_top_k output and calculate similarity
            search_results = []
            for row in rows:
                image_path = row[0]
                filename = row[1]
                embedding_bytes = row[2]

                # Calculate cosine similarity
                stored_embedding = np.frombuffer(embedding_bytes, dtype=np.float32)
                similarity = float(clip_service.similarity(query_embedding, stored_embedding))

                search_results.append(ImageSearchResult(
                    image_path=image_path,
                    filename=filename,
                    similarity_score=similarity
                ))

        return ImageSearchResponse(
            results=search_results,
            query=request.query,
            model_used=clip_service.model_name
        )

    except Exception as e:
        logger.error(f"Image search error: {e}", exc_info=True)
        raise HTTPException(status_code=500, detail=str(e))

@router.post("/headlines", response_model=HeadlineSearchResponse)
async def headline_search(request: HeadlineSearchRequest):
    """Search or list all headlines."""
    try:
        cursor = db.conn.cursor()

        # Build base query
        base_query = """
            SELECT h.title, f.filename, h.begin, h.level, h.tags, h.todo_keyword
            FROM headlines h
            JOIN files f ON h.filename_id = f.rowid
        """

        params = []
        where_clauses = []

        # Add title search if query provided
        if request.query:
            where_clauses.append("h.title LIKE ?")
            params.append(f"%{request.query}%")

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
        if where_clauses:
            base_query += " WHERE " + " AND ".join(where_clauses)

        base_query += " ORDER BY f.filename, h.begin LIMIT ?"
        params.append(request.limit)

        cursor.execute(base_query, params)
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
