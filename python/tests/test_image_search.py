"""Test image search API endpoint."""
import pytest
from fastapi.testclient import TestClient
from pathlib import Path

from org_db_server.main import app
from org_db_server.services.database import Database
from org_db_server.api import indexing

# Try to import CLIP service to check availability
try:
    from org_db_server.services.clip_service import get_clip_service
    clip_service = get_clip_service()
    CLIP_AVAILABLE = True
except Exception:
    CLIP_AVAILABLE = False


@pytest.fixture
def client(tmp_path):
    """Test client with temporary database."""
    test_db_path = tmp_path / "test.db"
    db = Database(test_db_path)

    # Override the global db instance in indexing module
    old_indexing_db = indexing.db
    indexing.db = db

    client = TestClient(app)
    yield client

    # Restore original db
    indexing.db = old_indexing_db
    db.close()


@pytest.mark.skipif(not CLIP_AVAILABLE, reason="CLIP service not available")
def test_image_search_endpoint(client, tmp_path):
    """Test that image search endpoint works."""
    # First, index a file with images
    payload = {
        "filename": "/test/images.org",
        "md5": "img123",
        "file_size": 512,
        "headlines": [
            {
                "title": "Sample Image",
                "level": 1,
                "begin": 1,
            }
        ],
        "links": [],
        "keywords": [],
        "src_blocks": [],
        "images": []  # Empty for now since we don't have actual images
    }

    response = client.post("/api/index/file", json=payload)
    assert response.status_code == 200

    # Query the image search endpoint
    search_payload = {
        "query": "sample image",
        "limit": 10
    }

    response = client.post("/api/search/images", json=search_payload)
    if response.status_code != 200:
        print("Error response:", response.json())
    assert response.status_code == 200

    data = response.json()
    assert "results" in data
    assert "query" in data
    assert "model_used" in data
    assert data["query"] == "sample image"
    assert isinstance(data["results"], list)
