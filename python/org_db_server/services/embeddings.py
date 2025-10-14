"""Embedding generation service."""
import numpy as np
from typing import List, Union
from sentence_transformers import SentenceTransformer

class EmbeddingService:
    """Service for generating text embeddings."""

    def __init__(self, model_name: str = "all-MiniLM-L6-v2"):
        """Initialize with a specific model."""
        self.model_name = model_name
        self.model = SentenceTransformer(model_name)
        self.dimension = self.model.get_sentence_embedding_dimension()

    def generate_embedding(self, text: str) -> np.ndarray:
        """Generate embedding for a single text."""
        return self.model.encode(text, convert_to_numpy=True)

    def generate_embeddings(self, texts: List[str], batch_size: int = 8) -> List[np.ndarray]:
        """Generate embeddings for multiple texts in batches.

        Reduced default batch size from 32 to 8 to use less memory during processing.
        """
        embeddings = self.model.encode(
            texts,
            batch_size=batch_size,
            convert_to_numpy=True,
            show_progress_bar=len(texts) > 100
        )
        return [embeddings[i] for i in range(len(texts))]

    def similarity(self, emb1: np.ndarray, emb2: np.ndarray) -> float:
        """Calculate cosine similarity between two embeddings."""
        return float(np.dot(emb1, emb2) / (np.linalg.norm(emb1) * np.linalg.norm(emb2)))

# Global embedding service instance (lazy loaded)
_embedding_service = None

def get_embedding_service(model_name: str = "all-MiniLM-L6-v2") -> EmbeddingService:
    """Get or create the global embedding service."""
    global _embedding_service
    if _embedding_service is None or _embedding_service.model_name != model_name:
        _embedding_service = EmbeddingService(model_name)
    return _embedding_service
