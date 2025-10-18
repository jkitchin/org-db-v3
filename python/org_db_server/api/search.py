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
    HeadlineSearchRequest, HeadlineSearchResponse, HeadlineSearchResult,
    PropertySearchRequest, PropertySearchResponse, PropertySearchResult
)
from org_db_server.services.database import Database
from org_db_server.services.embeddings import get_embedding_service
from org_db_server.services.clip_service import get_clip_service
from org_db_server.services.reranker import get_reranker_service
from org_db_server.config import settings

router = APIRouter(prefix="/api/search", tags=["search"])

# Global database instance
db = Database(settings.db_path, settings.semantic_db_path, settings.image_db_path)

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

        # Use semantic database connection
        cursor = db.semantic_conn.cursor()

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

            # Handle keyword filtering by querying main DB first
            matching_filenames = None
            if request.keyword:
                main_cursor = db.main_conn.cursor()
                main_cursor.execute("""
                    SELECT f.filename FROM files f
                    JOIN file_keywords fk ON f.rowid = fk.filename_id
                    JOIN keywords k ON fk.keyword_id = k.rowid
                    WHERE k.keyword = ?
                """, [request.keyword])
                matching_filenames = {row[0] for row in main_cursor.fetchall()}
                if not matching_filenames:
                    return SemanticSearchResponse(
                        results=[],
                        query=request.query,
                        model_used=model_name
                    )

            # Query semantic DB (no JOINs with main DB)
            base_query = """
                SELECT
                    e.chunk_id,
                    e.embedding_vector,
                    c.chunk_text,
                    c.chunk_type,
                    c.begin_line,
                    c.end_line,
                    c.filename,
                    c.linked_file_path,
                    '' as linked_file_type
                FROM embeddings e
                JOIN chunks c ON e.chunk_id = c.rowid
            """

            params = [model_name]
            where_clauses = ["e.embedding_model = ?"]

            if request.filename_pattern:
                where_clauses.append("c.filename LIKE ?")
                params.append(request.filename_pattern)

            # Apply filename filter from keyword search
            if matching_filenames:
                placeholders = ','.join('?' * len(matching_filenames))
                where_clauses.append(f"c.filename IN ({placeholders})")
                params.extend(matching_filenames)

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
                # Handle keyword filtering by querying main DB first
                matching_filenames = None
                if request.keyword:
                    main_cursor = db.main_conn.cursor()
                    main_cursor.execute("""
                        SELECT f.filename FROM files f
                        JOIN file_keywords fk ON f.rowid = fk.filename_id
                        JOIN keywords k ON fk.keyword_id = k.rowid
                        WHERE k.keyword = ?
                    """, [request.keyword])
                    matching_filenames = {row[0] for row in main_cursor.fetchall()}
                    if not matching_filenames:
                        return SemanticSearchResponse(
                            results=[],
                            query=request.query,
                            model_used=model_name
                        )

                # Build WHERE clause for filtering (exclude embedding_model - will filter in Python)
                where_parts = []
                where_params = []

                if request.filename_pattern:
                    where_parts.append("c.filename LIKE ?")
                    where_params.append(request.filename_pattern)

                if matching_filenames:
                    placeholders = ','.join('?' * len(matching_filenames))
                    where_parts.append(f"c.filename IN ({placeholders})")
                    where_params.extend(matching_filenames)

                # Get more candidates from vector search and filter
                # NOTE: We can't filter by embedding_model in WHERE clause as it breaks vector index usage
                # Instead, we over-fetch and filter in Python
                if where_parts:
                    where_clause = " AND ".join(where_parts)
                    cursor.execute(f"""
                        SELECT
                            vt.id as chunk_id,
                            c.chunk_text,
                            c.chunk_type,
                            c.begin_line,
                            c.end_line,
                            c.filename,
                            c.linked_file_path,
                            '' as linked_file_type,
                            e.embedding_vector,
                            e.embedding_model
                        FROM vector_top_k('idx_embeddings_vector', ?, ?) vt
                        JOIN embeddings e ON e.rowid = vt.id
                        JOIN chunks c ON c.rowid = e.chunk_id
                        WHERE {where_clause}
                    """, [query_bytes, num_candidates * 3] + where_params)
                else:
                    # No WHERE filters, just fetch all
                    cursor.execute("""
                        SELECT
                            vt.id as chunk_id,
                            c.chunk_text,
                            c.chunk_type,
                            c.begin_line,
                            c.end_line,
                            c.filename,
                            c.linked_file_path,
                            '' as linked_file_type,
                            e.embedding_vector,
                            e.embedding_model
                        FROM vector_top_k('idx_embeddings_vector', ?, ?) vt
                        JOIN embeddings e ON e.rowid = vt.id
                        JOIN chunks c ON c.rowid = e.chunk_id
                    """, [query_bytes, num_candidates * 3])

                # Filter results by embedding_model in Python
                all_rows = cursor.fetchall()
                rows = [row for row in all_rows if row[9] == model_name][:num_candidates]
                logger.info(f"Vector search with filters: {(time.perf_counter()-t1)*1000:.1f}ms")
            else:
                # No filters: use vector_top_k with model filtering in Python
                # NOTE: We can't filter by embedding_model in WHERE clause as it breaks vector index usage
                # Instead, we over-fetch and filter in Python
                cursor.execute("""
                    SELECT
                        vt.id as chunk_id,
                        c.chunk_text,
                        c.chunk_type,
                        c.begin_line,
                        c.end_line,
                        c.filename,
                        c.linked_file_path,
                        '' as linked_file_type,
                        e.embedding_vector,
                        e.embedding_model
                    FROM vector_top_k('idx_embeddings_vector', ?, ?) vt
                    JOIN embeddings e ON e.rowid = vt.id
                    JOIN chunks c ON c.rowid = e.chunk_id
                """, [query_bytes, num_candidates * 3])  # Get 3x more results to account for filtering

                # Filter results by embedding_model in Python
                all_rows = cursor.fetchall()
                rows = [row for row in all_rows if row[9] == model_name][:num_candidates]
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
        cursor = db.main_conn.cursor()

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

        cursor = db.image_conn.cursor()

        # Count total images to decide between exact vs ANN search
        cursor.execute("SELECT COUNT(*) FROM image_embeddings WHERE clip_model = ?",
                      [clip_service.model_name])
        total_images = cursor.fetchone()[0]

        # Use exact search for small-to-medium datasets (more accurate and faster up to ~2000 images)
        # Use ANN search only for very large datasets (>2000 images)
        # NOTE: ANN search has a bug - WHERE clause after vector_top_k doesn't use index properly
        # This makes ANN search very slow. Until fixed, use exact search for medium datasets.
        use_exact_search = total_images < 5000

        if use_exact_search:
            # Exact search: fetch all embeddings and calculate similarities
            logger.debug(f"Using exact search for {total_images} images")

            # Handle keyword filtering by querying main DB first
            matching_filenames = None
            if request.keyword:
                main_cursor = db.main_conn.cursor()
                main_cursor.execute("""
                    SELECT f.filename FROM files f
                    JOIN file_keywords fk ON f.rowid = fk.filename_id
                    JOIN keywords k ON fk.keyword_id = k.rowid
                    WHERE k.keyword = ?
                """, [request.keyword])
                matching_filenames = {row[0] for row in main_cursor.fetchall()}
                if not matching_filenames:
                    return ImageSearchResponse(
                        results=[],
                        query=request.query,
                        model_used=clip_service.model_name
                    )

            # Query image DB (no JOINs with main DB)
            base_query = """
                SELECT
                    ie.rowid,
                    ie.embedding_vector,
                    i.image_path,
                    i.filename
                FROM image_embeddings ie
                JOIN images i ON ie.image_id = i.rowid
            """

            params = [clip_service.model_name]
            where_clauses = ["ie.clip_model = ?"]

            if request.filename_pattern:
                where_clauses.append("i.filename LIKE ?")
                params.append(request.filename_pattern)

            # Apply filename filter from keyword search
            if matching_filenames:
                placeholders = ','.join('?' * len(matching_filenames))
                where_clauses.append(f"i.filename IN ({placeholders})")
                params.extend(matching_filenames)

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
                # Handle keyword filtering by querying main DB first
                matching_filenames = None
                if request.keyword:
                    main_cursor = db.main_conn.cursor()
                    main_cursor.execute("""
                        SELECT f.filename FROM files f
                        JOIN file_keywords fk ON f.rowid = fk.filename_id
                        JOIN keywords k ON fk.keyword_id = k.rowid
                        WHERE k.keyword = ?
                    """, [request.keyword])
                    matching_filenames = {row[0] for row in main_cursor.fetchall()}
                    if not matching_filenames:
                        return ImageSearchResponse(
                            results=[],
                            query=request.query,
                            model_used=clip_service.model_name
                        )

                # Build WHERE clause for filtering (exclude clip_model - will filter in Python)
                where_parts = []
                where_params = []

                if request.filename_pattern:
                    where_parts.append("i.filename LIKE ?")
                    where_params.append(request.filename_pattern)

                if matching_filenames:
                    placeholders = ','.join('?' * len(matching_filenames))
                    where_parts.append(f"i.filename IN ({placeholders})")
                    where_params.extend(matching_filenames)

                # Get results from vector search filtered by filenames
                # NOTE: We can't filter by clip_model in WHERE clause as it breaks vector index usage
                # Instead, we over-fetch and filter in Python
                if where_parts:
                    where_clause = " AND ".join(where_parts)
                    cursor.execute(f"""
                        SELECT
                            ie.rowid,
                            i.image_path,
                            i.filename,
                            ie.embedding_vector,
                            ie.clip_model
                        FROM vector_top_k('idx_image_embeddings_vector', ?, ?) vt
                        JOIN image_embeddings ie ON ie.rowid = vt.id
                        JOIN images i ON ie.image_id = i.rowid
                        WHERE {where_clause}
                    """, [query_bytes, request.limit * 3] + where_params)
                else:
                    # Shouldn't reach here, but handle gracefully
                    cursor.execute("""
                        SELECT
                            ie.rowid,
                            i.image_path,
                            i.filename,
                            ie.embedding_vector,
                            ie.clip_model
                        FROM vector_top_k('idx_image_embeddings_vector', ?, ?) vt
                        JOIN image_embeddings ie ON ie.rowid = vt.id
                        JOIN images i ON ie.image_id = i.rowid
                    """, [query_bytes, request.limit * 3])

                # Filter results by clip_model in Python
                all_rows = cursor.fetchall()
                rows = [row for row in all_rows if row[4] == clip_service.model_name][:request.limit]
            else:
                # No filters: use vector_top_k with model filtering
                # Strategy: Get more results from vector_top_k, then filter by model in Python
                # This is necessary because WHERE clause after vector_top_k doesn't use the index properly
                cursor.execute("""
                    SELECT
                        ie.rowid,
                        i.image_path,
                        i.filename,
                        ie.embedding_vector,
                        ie.clip_model
                    FROM vector_top_k('idx_image_embeddings_vector', ?, ?) vt
                    JOIN image_embeddings ie ON ie.rowid = vt.id
                    JOIN images i ON ie.image_id = i.rowid
                """, [query_bytes, request.limit * 3])  # Get 3x more results to account for filtering

                # Filter results by clip_model in Python
                all_rows = cursor.fetchall()
                rows = [row for row in all_rows if row[4] == clip_service.model_name][:request.limit]

            if not rows:
                return ImageSearchResponse(
                    results=[],
                    query=request.query,
                    model_used=clip_service.model_name
                )

            # Build results from vector_top_k output and calculate similarity
            # Columns: rowid, image_path, filename, embedding_vector, clip_model
            search_results = []
            for row in rows:
                image_path = row[1]
                filename = row[2]
                embedding_bytes = row[3]

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
        cursor = db.main_conn.cursor()

        # Build base query - format display string directly in SQL for performance
        # SQLite's printf is much faster than Python string formatting for 100K+ rows
        base_query = """
            SELECT
                substr(h.title, 1, 80) || printf('%*s', max(0, 80 - length(h.title)), '') || ' | ' || f.filename as display_string,
                f.filename,
                h.begin
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

        # Dynamic ORDER BY based on sort_by parameter
        if request.sort_by == "last_updated":
            base_query += " ORDER BY f.last_updated DESC, h.begin"
        elif request.sort_by == "indexed_at":
            base_query += " ORDER BY f.indexed_at DESC, h.begin"
        else:  # default to "filename"
            base_query += " ORDER BY f.filename, h.begin"

        # Add LIMIT only if specified
        if request.limit is not None:
            base_query += " LIMIT ?"
            params.append(request.limit)

        cursor.execute(base_query, params)
        rows = cursor.fetchall()

        # SQL already formatted the display strings - just convert to list
        # Returns [display_string, filename, begin] for each result
        results = [[row[0], row[1], row[2]] for row in rows]

        return HeadlineSearchResponse(
            results=results,
            query=request.query
        )

    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

@router.post("/properties", response_model=PropertySearchResponse)
async def property_search(request: PropertySearchRequest):
    """Search headlines by property name and optionally value."""
    try:
        cursor = db.main_conn.cursor()

        # Build base query to find headlines with specific properties
        base_query = """
            SELECT h.title, f.filename, h.begin, p.property, hp.value
            FROM headline_properties hp
            JOIN headlines h ON hp.headline_id = h.rowid
            JOIN files f ON h.filename_id = f.rowid
            JOIN properties p ON hp.property_id = p.rowid
        """

        params = []
        where_clauses = ["p.property = ?"]
        params.append(request.property)

        # Add value filter if provided
        if request.value:
            where_clauses.append("hp.value LIKE ?")
            params.append(f"%{request.value}%")

        # Add filename pattern filter if provided
        if request.filename_pattern:
            where_clauses.append("f.filename LIKE ?")
            params.append(request.filename_pattern)

        # Combine WHERE clauses
        base_query += " WHERE " + " AND ".join(where_clauses)
        base_query += " ORDER BY f.filename, h.begin LIMIT ?"
        params.append(request.limit)

        cursor.execute(base_query, params)
        rows = cursor.fetchall()

        # Convert to result objects
        results = [
            PropertySearchResult(
                headline_title=row[0],
                filename=row[1],
                begin=row[2],
                property=row[3],
                value=row[4]
            )
            for row in rows
        ]

        return PropertySearchResponse(
            results=results,
            property=request.property
        )

    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))
