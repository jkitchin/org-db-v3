"""Tests for text chunking."""
import pytest
from org_db_server.services.chunking import chunk_text

def test_chunk_by_paragraphs():
    """Test chunking text into paragraphs."""
    text = """This is paragraph one.

This is paragraph two with more content.

This is paragraph three."""

    chunks = chunk_text(text, method="paragraph")

    assert len(chunks) == 3
    assert chunks[0]["text"].startswith("This is paragraph one")
    assert chunks[0]["chunk_type"] == "paragraph"

def test_chunk_respects_size_limit():
    """Test that chunks respect maximum size."""
    text = "word " * 1000  # Very long text

    chunks = chunk_text(text, method="fixed", chunk_size=100, chunk_overlap=10)

    for chunk in chunks:
        assert len(chunk["text"]) <= 120  # Size + some overlap
