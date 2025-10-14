"""Configuration management for org-db server."""
from pathlib import Path
from typing import Optional
from pydantic_settings import BaseSettings

class Settings(BaseSettings):
    """Application settings."""

    # Server
    host: str = "127.0.0.1"
    port: int = 8765

    # Database
    db_path: Path = Path.home() / "org-db" / "org-db-v3.db"

    # Embedding models
    default_embedding_model: str = "all-MiniLM-L6-v2"
    default_clip_model: str = "clip-ViT-B-32"

    # Indexing
    chunk_size: int = 512
    chunk_overlap: int = 50

    # Feature flags
    enable_linked_files: bool = True  # Now using subprocess isolation for safety

    class Config:
        env_prefix = "ORG_DB_"

settings = Settings()

# Ensure database directory exists
settings.db_path.parent.mkdir(parents=True, exist_ok=True)
