"""Tests for database schema completeness."""
import pytest
from pathlib import Path
from org_db_server.services.database import Database

@pytest.fixture
def temp_db(tmp_path):
    """Create a temporary database for testing."""
    db_path = tmp_path / "test.db"
    db = Database(db_path)
    yield db
    db.close()

def test_all_core_tables_exist(temp_db):
    """Test that all core tables are created."""
    cursor = temp_db.conn.cursor()

    # List of all expected tables
    expected_tables = [
        'files', 'headlines', 'tags', 'headline_tags', 'properties',
        'headline_properties', 'keywords', 'file_keywords', 'links',
        'hashtags', 'file_hashtags', 'atlabels', 'file_atlabels',
        'email_addresses', 'file_email_addresses', 'src_blocks',
        'timestamps', 'linked_files', 'chunks', 'embeddings', 'images', 'image_embeddings',
        'fts_content', 'schema_version'
    ]

    for table_name in expected_tables:
        cursor.execute(
            "SELECT name FROM sqlite_master WHERE type='table' AND name=?",
            (table_name,)
        )
        result = cursor.fetchone()
        assert result is not None, f"Table {table_name} does not exist"

def test_foreign_keys_enabled(temp_db):
    """Test that foreign keys are enabled."""
    cursor = temp_db.conn.cursor()
    cursor.execute("PRAGMA foreign_keys")
    result = cursor.fetchone()
    assert result[0] == 1, "Foreign keys should be enabled"

def test_files_table_structure(temp_db):
    """Test files table has correct columns."""
    cursor = temp_db.conn.cursor()
    cursor.execute("PRAGMA table_info(files)")
    columns = {row[1] for row in cursor.fetchall()}

    expected_columns = {'rowid', 'filename', 'md5', 'last_updated', 'file_size', 'indexed_at'}
    # rowid is implicit, check the explicit columns
    explicit_columns = {'filename', 'md5', 'last_updated', 'file_size', 'indexed_at'}
    assert explicit_columns.issubset(columns), f"Missing columns in files table: {explicit_columns - columns}"

def test_headlines_table_structure(temp_db):
    """Test headlines table has correct columns."""
    cursor = temp_db.conn.cursor()
    cursor.execute("PRAGMA table_info(headlines)")
    columns = {row[1] for row in cursor.fetchall()}

    expected_columns = {
        'filename_id', 'title', 'level', 'todo_keyword', 'todo_type',
        'archivedp', 'commentedp', 'begin', 'end', 'tags', 'priority',
        'scheduled', 'deadline'
    }
    assert expected_columns.issubset(columns), f"Missing columns in headlines table: {expected_columns - columns}"

def test_chunks_and_embeddings_tables(temp_db):
    """Test that chunks and embeddings tables exist with proper structure."""
    cursor = temp_db.conn.cursor()

    # Check chunks table
    cursor.execute("PRAGMA table_info(chunks)")
    chunk_columns = {row[1] for row in cursor.fetchall()}
    expected_chunk_cols = {
        'filename_id', 'headline_id', 'chunk_text', 'chunk_type',
        'begin_line', 'end_line', 'char_offset', 'linked_file_id'
    }
    assert expected_chunk_cols.issubset(chunk_columns), f"Missing columns in chunks table: {expected_chunk_cols - chunk_columns}"

    # Check embeddings table
    cursor.execute("PRAGMA table_info(embeddings)")
    embedding_columns = {row[1] for row in cursor.fetchall()}
    expected_embedding_cols = {
        'chunk_id', 'embedding_model', 'embedding_vector', 'embedding_dim', 'created_at'
    }
    assert expected_embedding_cols.issubset(embedding_columns), f"Missing columns in embeddings table: {expected_embedding_cols - embedding_columns}"

def test_linked_files_table(temp_db):
    """Test that linked_files table exists with proper structure."""
    cursor = temp_db.conn.cursor()

    cursor.execute("PRAGMA table_info(linked_files)")
    columns = {row[1] for row in cursor.fetchall()}

    expected_columns = {
        'org_file_id', 'org_link_line', 'file_path', 'file_type', 'file_size',
        'md5', 'last_converted', 'conversion_status', 'conversion_error', 'indexed_at'
    }
    assert expected_columns.issubset(columns), f"Missing columns in linked_files table: {expected_columns - columns}"

def test_images_tables_exist(temp_db):
    """Test that image-related tables exist."""
    cursor = temp_db.conn.cursor()

    # Check images table
    cursor.execute("PRAGMA table_info(images)")
    image_columns = {row[1] for row in cursor.fetchall()}
    expected_image_cols = {
        'filename_id', 'image_path', 'image_type', 'width', 'height',
        'file_size', 'begin', 'ocr_text'
    }
    assert expected_image_cols.issubset(image_columns), f"Missing columns in images table"

    # Check image_embeddings table
    cursor.execute("PRAGMA table_info(image_embeddings)")
    image_emb_columns = {row[1] for row in cursor.fetchall()}
    expected_image_emb_cols = {
        'image_id', 'clip_model', 'embedding_vector', 'embedding_dim', 'created_at'
    }
    assert expected_image_emb_cols.issubset(image_emb_columns), f"Missing columns in image_embeddings table"
