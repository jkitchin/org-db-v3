"""Tests for indexing API endpoints."""
import pytest
from fastapi.testclient import TestClient
from pathlib import Path

from org_db_server.main import app
from org_db_server.services.database import Database
from org_db_server.config import settings
from org_db_server.api import indexing

@pytest.fixture
def client(tmp_path):
    """Create test client with temporary database."""
    # Override database path for testing
    test_db_path = tmp_path / "test.db"

    # Create database instance
    db = Database(test_db_path)

    # Override the global db instances
    old_indexing_db = indexing.db
    indexing.db = db

    client = TestClient(app)
    yield client

    # Restore
    indexing.db = old_indexing_db
    db.close()

def test_index_file_endpoint(client):
    """Test POST /api/index/file endpoint."""
    payload = {
        "filename": "/test/sample.org",
        "md5": "abc123",
        "file_size": 1024,
        "headlines": [
            {
                "title": "Test Heading",
                "level": 1,
                "todo_keyword": "TODO",
                "tags": ":test:",
                "begin": 10,
                "end": 50
            }
        ],
        "links": [],
        "keywords": [
            {"key": "TITLE", "value": "Test", "begin": 0}
        ],
        "src_blocks": []
    }

    response = client.post("/api/index/file", json=payload)

    assert response.status_code == 200
    data = response.json()
    assert "file_id" in data
    assert data["status"] == "indexed"

def test_fts_populated_during_indexing(client, tmp_path):
    """Test that FTS5 table is populated when indexing."""
    # Index a file with content
    payload = {
        "filename": "/test/fts_test.org",
        "md5": "fts123",
        "file_size": 512,
        "headlines": [
            {
                "title": "Machine Learning Introduction",
                "level": 1,
                "todo_keyword": None,
                "tags": ":ai:ml:",
                "begin": 10,
                "end": 200
            },
            {
                "title": "Deep Neural Networks",
                "level": 2,
                "todo_keyword": None,
                "tags": ":ai:",
                "begin": 201,
                "end": 400
            }
        ],
        "links": [],
        "keywords": [],
        "src_blocks": []
    }

    response = client.post("/api/index/file", json=payload)
    assert response.status_code == 200

    # Query FTS5 table to verify content was indexed
    db = Database(tmp_path / "test.db")
    cursor = db.conn.cursor()

    # Search for "machine"
    cursor.execute("SELECT * FROM fts_content WHERE fts_content MATCH 'machine'")
    results = cursor.fetchall()
    assert len(results) > 0

    # Search for "neural"
    cursor.execute("SELECT * FROM fts_content WHERE fts_content MATCH 'neural'")
    results = cursor.fetchall()
    assert len(results) > 0

    db.close()
