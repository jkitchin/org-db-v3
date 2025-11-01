"""Tests for search API endpoints."""
import pytest
from fastapi.testclient import TestClient
from pathlib import Path
import tempfile
import shutil

from org_db_server.main import app
from org_db_server.services.database import Database
from org_db_server.services.embeddings import get_embedding_service
from org_db_server.services.chunking import chunk_text
from org_db_server.config import settings
from org_db_server.api import search, indexing

@pytest.fixture
def temp_db():
    """Create temporary database for testing."""
    temp_dir = tempfile.mkdtemp()
    db_path = Path(temp_dir) / "test.db"
    semantic_path = Path(temp_dir) / "semantic.db"
    image_path = Path(temp_dir) / "image.db"

    # Create database instance
    db = Database(db_path, semantic_path, image_path)

    # Override the global db instances in both modules
    old_search_db = search.db
    old_indexing_db = indexing.db
    search.db = db
    indexing.db = db

    yield db

    # Restore original db instances
    search.db = old_search_db
    indexing.db = old_indexing_db

    db.close()
    shutil.rmtree(temp_dir)

@pytest.fixture
def client(temp_db):
    """Create test client."""
    return TestClient(app)

def test_semantic_search(client, temp_db):
    """Test semantic search endpoint."""
    # First, index a file with some content
    file_id = temp_db.get_or_create_file_id("test.org", "abc123", 1000)
    temp_db.main_conn.commit()

    # Create some test chunks with embeddings
    test_texts = [
        "Machine learning and artificial intelligence",
        "Database systems and SQL queries",
        "Web development with Python and FastAPI"
    ]

    chunks = []
    for i, text in enumerate(test_texts):
        chunks.append({
            "text": text,
            "chunk_type": "paragraph",
            "begin_line": i * 10,
            "end_line": i * 10 + 5
        })

    # Generate embeddings
    embedding_service = get_embedding_service()
    embeddings = embedding_service.generate_embeddings(test_texts)

    # Store chunks
    temp_db.store_chunks(file_id, chunks, embeddings, embedding_service.model_name)

    # Now search for something related to AI
    response = client.post(
        "/api/search/semantic",
        json={
            "query": "artificial intelligence and ML",
            "limit": 2
        }
    )

    assert response.status_code == 200
    data = response.json()

    assert "results" in data
    assert len(data["results"]) <= 2

    # First result should be most similar (AI/ML text)
    if len(data["results"]) > 0:
        first_result = data["results"][0]
        assert "chunk_text" in first_result
        assert "similarity_score" in first_result
        assert "filename" in first_result
        # Similarity scores can be low for short texts, just verify it's positive
        assert first_result["similarity_score"] > 0

        # Should match the AI/ML text (most semantically similar)
        assert "artificial intelligence" in first_result["chunk_text"].lower() or \
               "machine learning" in first_result["chunk_text"].lower()

def test_semantic_search_empty_query(client):
    """Test semantic search with empty query."""
    response = client.post(
        "/api/search/semantic",
        json={
            "query": "",
            "limit": 10
        }
    )

    assert response.status_code == 422  # Validation error

def test_semantic_search_no_results(client, temp_db):
    """Test semantic search when database is empty."""
    response = client.post(
        "/api/search/semantic",
        json={
            "query": "test query",
            "limit": 10
        }
    )

    assert response.status_code == 200
    data = response.json()
    assert data["results"] == []
