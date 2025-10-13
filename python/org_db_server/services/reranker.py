"""Cross-encoder reranking service for semantic search."""
from sentence_transformers import CrossEncoder
from typing import List, Tuple, Optional
import logging

logger = logging.getLogger(__name__)

class RerankerService:
    """Service for reranking search results using cross-encoder models."""

    def __init__(self, model_name: str = "cross-encoder/ms-marco-MiniLM-L-6-v2"):
        """Initialize the reranker with a cross-encoder model.

        Args:
            model_name: Name of the cross-encoder model to use.
                       Default is ms-marco-MiniLM-L-6-v2 which is fast and accurate.
        """
        self.model_name = model_name
        self._model: Optional[CrossEncoder] = None
        logger.info(f"Reranker service initialized with model: {model_name}")

    @property
    def model(self) -> CrossEncoder:
        """Lazy-load the model on first use."""
        if self._model is None:
            logger.info(f"Loading cross-encoder model: {self.model_name}")
            self._model = CrossEncoder(self.model_name)
            logger.info("Cross-encoder model loaded successfully")
        return self._model

    def rerank(
        self,
        query: str,
        results: List[dict],
        text_field: str = "chunk_text",
        score_field: str = "similarity_score",
        top_k: Optional[int] = None
    ) -> List[dict]:
        """Rerank search results using cross-encoder.

        Args:
            query: The search query
            results: List of result dictionaries to rerank
            text_field: Name of the field containing text to rerank on
            score_field: Name of the field to store the new score in
            top_k: Number of top results to return after reranking (None = all)

        Returns:
            List of reranked results with updated scores
        """
        if not results:
            return results

        # Prepare query-document pairs
        pairs = [(query, result[text_field]) for result in results]

        # Get cross-encoder scores
        scores = self.model.predict(pairs)

        # Update results with new scores and sort
        reranked = []
        for result, score in zip(results, scores):
            result_copy = result.copy()
            result_copy[score_field] = float(score)
            result_copy["reranked"] = True
            reranked.append(result_copy)

        # Sort by new scores (highest first)
        reranked.sort(key=lambda x: x[score_field], reverse=True)

        # Return top K if specified
        if top_k is not None:
            return reranked[:top_k]
        return reranked


# Global reranker instance
_reranker_service: Optional[RerankerService] = None


def get_reranker_service(model_name: str = "cross-encoder/ms-marco-MiniLM-L-6-v2") -> RerankerService:
    """Get or create the global reranker service instance.

    Args:
        model_name: Name of the cross-encoder model to use

    Returns:
        RerankerService instance
    """
    global _reranker_service
    if _reranker_service is None or _reranker_service.model_name != model_name:
        _reranker_service = RerankerService(model_name)
    return _reranker_service
