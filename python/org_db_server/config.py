"""Configuration management for org-db server."""
from pathlib import Path
from typing import Optional
from pydantic_settings import BaseSettings

class Settings(BaseSettings):
    """Application settings."""

    # Server
    host: str = "127.0.0.1"
    port: int = 8765

    # Databases - separated for performance and size management
    # Main database: metadata, headlines, links, properties, FTS5
    db_path: Path = Path.home() / "org-db" / "org-db-v3.db"

    # Semantic search database: text chunks and embeddings (with libsql vector search)
    semantic_db_path: Path = Path.home() / "org-db" / "org-db-v3-semantic.db"

    # Image search database: images and CLIP embeddings (with libsql vector search)
    image_db_path: Path = Path.home() / "org-db" / "org-db-v3-images.db"

    # Embedding models
    default_embedding_model: str = "all-MiniLM-L6-v2"
    default_clip_model: str = "clip-ViT-B-32"

    # Indexing for org files - using aggressive chunking to reduce DB size
    org_chunk_method: str = "fixed"  # "paragraph" or "fixed"
    org_chunk_size: int = 2048  # Aggressive: larger chunks = fewer embeddings
    org_chunk_overlap: int = 200  # Overlap for context

    # Feature flags
    enable_linked_files: bool = False  # Disabled to prevent database bloat

    # Linked file limits (to prevent database bloat)
    max_linked_file_size_mb: int = 20  # Skip files larger than this (0 = no limit)
    max_linked_file_chunks: int = 50   # Limit chunks per file (0 = no limit)
    linked_file_chunk_size: int = 2048  # Larger chunks = fewer embeddings
    linked_file_chunk_overlap: int = 200

    class Config:
        env_prefix = "ORG_DB_"

settings = Settings()

# Ensure database directories exist
settings.db_path.parent.mkdir(parents=True, exist_ok=True)
settings.semantic_db_path.parent.mkdir(parents=True, exist_ok=True)
settings.image_db_path.parent.mkdir(parents=True, exist_ok=True)
