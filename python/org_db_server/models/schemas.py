"""Pydantic models for API requests/responses."""
from typing import List, Optional, Dict, Any
from pydantic import BaseModel, Field

class HeadlineData(BaseModel):
    """Headline data from Emacs."""
    title: str
    level: int
    todo_keyword: Optional[str] = None
    todo_type: Optional[str] = None
    archivedp: Optional[bool] = None
    commentedp: Optional[bool] = None
    begin: int
    end: Optional[int] = None
    tags: Optional[str] = None
    priority: Optional[str] = None
    scheduled: Optional[str] = None
    deadline: Optional[str] = None
    properties: Optional[Dict[str, str]] = None

class LinkData(BaseModel):
    """Link data from Emacs."""
    type: str
    path: str
    raw_link: str
    description: Optional[str] = None
    search_option: Optional[str] = None
    begin: int

class KeywordData(BaseModel):
    """Keyword data from Emacs."""
    key: str
    value: str
    begin: int

class SrcBlockData(BaseModel):
    """Source block data from Emacs."""
    language: Optional[str] = None
    contents: str
    begin: int

class ImageData(BaseModel):
    """Image data from Emacs."""
    path: str
    begin: int

class IndexFileRequest(BaseModel):
    """Request to index a file."""
    filename: str
    md5: str
    file_size: int
    content: Optional[str] = None
    headlines: List[HeadlineData] = Field(default_factory=list)
    links: List[LinkData] = Field(default_factory=list)
    keywords: List[KeywordData] = Field(default_factory=list)
    src_blocks: List[SrcBlockData] = Field(default_factory=list)
    images: List[ImageData] = Field(default_factory=list)

class IndexFileResponse(BaseModel):
    """Response from indexing a file."""
    file_id: int
    status: str
    headlines_count: int
    links_count: int

class SemanticSearchRequest(BaseModel):
    """Request for semantic search."""
    query: str = Field(..., min_length=1, description="Search query text")
    limit: int = Field(default=10, ge=1, le=100, description="Maximum number of results")
    model: Optional[str] = Field(default=None, description="Embedding model to use (optional)")
    filename_pattern: Optional[str] = Field(default=None, description="SQL LIKE pattern for directory/project scope")
    keyword: Optional[str] = Field(default=None, description="Keyword/tag filter")
    rerank: bool = Field(default=False, description="Enable cross-encoder reranking for better accuracy")
    rerank_candidates: int = Field(default=50, ge=10, le=200, description="Number of candidates to retrieve before reranking")

class SearchResult(BaseModel):
    """Single search result."""
    chunk_id: int
    chunk_text: str
    similarity_score: float
    filename: str
    chunk_type: str
    begin_line: int
    end_line: int
    reranked: bool = False

class SemanticSearchResponse(BaseModel):
    """Response from semantic search."""
    results: List[SearchResult]
    query: str
    model_used: str
    reranked: bool = False  # True if results were reranked

class FulltextSearchRequest(BaseModel):
    """Request for full-text search."""
    query: str = Field(..., min_length=1, description="Search query (FTS5 syntax)")
    limit: int = Field(default=10, ge=1, le=100, description="Maximum number of results")
    filename_pattern: Optional[str] = Field(default=None, description="SQL LIKE pattern for directory/project scope")
    keyword: Optional[str] = Field(default=None, description="Keyword/tag filter")

class FulltextSearchResult(BaseModel):
    """Single fulltext search result."""
    filename: str
    title: str
    content: str
    tags: str
    snippet: str
    rank: float

class FulltextSearchResponse(BaseModel):
    """Response from fulltext search."""
    results: List[FulltextSearchResult]
    query: str

class ImageSearchRequest(BaseModel):
    """Request for image search by text description."""
    query: str = Field(..., min_length=1, description="Text description of image")
    limit: int = Field(default=10, ge=1, le=100, description="Maximum number of results")
    filename_pattern: Optional[str] = Field(default=None, description="SQL LIKE pattern for directory/project scope")
    keyword: Optional[str] = Field(default=None, description="Keyword/tag filter")

class ImageSearchResult(BaseModel):
    """Single image search result."""
    image_path: str
    similarity_score: float
    filename: str

class ImageSearchResponse(BaseModel):
    """Response from image search."""
    results: List[ImageSearchResult]
    query: str
    model_used: str

class HeadlineSearchRequest(BaseModel):
    """Request for headline search."""
    query: str = Field(default="", description="Search query (empty for all headlines)")
    limit: int = Field(default=100, ge=1, le=1000, description="Maximum number of results")
    filename_pattern: Optional[str] = Field(default=None, description="SQL LIKE pattern for directory/project scope")
    keyword: Optional[str] = Field(default=None, description="Keyword/tag filter")

class HeadlineSearchResult(BaseModel):
    """Single headline search result."""
    title: str
    filename: str
    begin: int
    level: int
    tags: Optional[str] = None
    todo_keyword: Optional[str] = None

class HeadlineSearchResponse(BaseModel):
    """Response from headline search."""
    results: List[HeadlineSearchResult]
    query: str
