"""Tests for embedding service."""
import pytest
import numpy as np
from org_db_server.services.embeddings import EmbeddingService

@pytest.fixture
def embedding_service():
    """Create embedding service with small model for testing."""
    return EmbeddingService(model_name="all-MiniLM-L6-v2")

def test_generate_embedding(embedding_service):
    """Test generating a single embedding."""
    text = "This is a test sentence."

    embedding = embedding_service.generate_embedding(text)

    assert isinstance(embedding, np.ndarray)
    assert embedding.shape[0] == 384  # all-MiniLM-L6-v2 dimension
    assert np.isfinite(embedding).all()

def test_generate_embeddings_batch(embedding_service):
    """Test generating embeddings in batch."""
    texts = ["First sentence.", "Second sentence.", "Third sentence."]

    embeddings = embedding_service.generate_embeddings(texts)

    assert len(embeddings) == 3
    assert all(emb.shape[0] == 384 for emb in embeddings)
