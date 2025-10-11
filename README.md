# org-db v3

A modern org-mode database with semantic search capabilities. Built with a hybrid architecture: Emacs handles org parsing and UI, while a Python FastAPI backend provides storage, embeddings, and vector search.

## Features

- **Semantic Search**: Find org content by meaning, not just keywords
- **Automatic Indexing**: Background indexing of org files via idle timers
- **Fast Vector Search**: Local embeddings using sentence-transformers
- **Non-blocking**: Async HTTP client keeps Emacs responsive
- **Extensible**: CLIP support for future image search (optional)

## Architecture

```
┌─────────────────┐         ┌──────────────────────┐
│  Emacs (UI)     │◄───────►│  FastAPI Server      │
│  - org parsing  │  HTTP   │  - SQLite storage    │
│  - search UI    │         │  - embeddings (ML)   │
│  - navigation   │         │  - vector search     │
└─────────────────┘         └──────────────────────┘
```

## Installation

### Prerequisites

- Emacs 28.1+
- Python 3.10+
- [uv](https://github.com/astral-sh/uv) (Python package manager)

### Python Backend

```bash
cd python
uv sync
```

This installs all dependencies including:
- FastAPI & Uvicorn (web server)
- sentence-transformers (embeddings)
- SQLite (database)

### Emacs Package

Add to your Emacs config:

```elisp
(add-to-list 'load-path "/path/to/org-db-v3/elisp")
(require 'org-db-v3)
(require 'org-db-v3-search)

;; Enable automatic indexing
(org-db-v3-enable)
```

Required Emacs packages:
- `plz` (async HTTP client)
- `transient` (menu system, for future UI)

Install via package manager or manually.

## Quick Start

### 1. Start the Server

From the `python` directory:

```bash
uv run uvicorn org_db_server.main:app --reload --port 8765
```

Or from Emacs:

```elisp
M-x org-db-v3-start-server
```

### 2. Index Your Org Files

Open any `.org` file in Emacs - it will be automatically added to the indexing queue. Files are indexed during idle time (non-blocking).

Manual indexing:

```elisp
M-x org-db-v3-index-file-async
```

### 3. Search

```elisp
M-x org-db-v3-semantic-search
```

Enter your query (e.g., "machine learning projects"). Results appear in an org-mode buffer with similarity scores.

**Search at point:**

```elisp
M-x org-db-v3-search-at-point
```

Uses selected region or sentence at point as query.

## Usage

### Search Results Navigation

In the `*org-db search*` buffer:

- `RET` - Jump to result location
- `n` - Next result
- `p` - Previous result
- `s` - New search
- `q` - Quit window

### Server Management

```elisp
;; Start server
M-x org-db-v3-start-server

;; Stop server
M-x org-db-v3-stop-server

;; Check server status
M-x org-db-v3-ensure-server
```

### Configuration

```elisp
;; Server connection
(setq org-db-v3-server-host "127.0.0.1")
(setq org-db-v3-server-port 8765)

;; Auto-start server
(setq org-db-v3-auto-start-server t)

;; Search defaults
(setq org-db-v3-search-default-limit 10)
```

## API Endpoints

The FastAPI server provides REST endpoints:

### Health Check

```bash
GET /health
```

### Index File

```bash
POST /api/index/file
Content-Type: application/json

{
  "filename": "/path/to/file.org",
  "md5": "abc123...",
  "file_size": 1024,
  "headlines": [...],
  "links": [...],
  "keywords": [...],
  "src_blocks": [...]
}
```

### Semantic Search

```bash
POST /api/search/semantic
Content-Type: application/json

{
  "query": "your search query",
  "limit": 10,
  "model": "all-MiniLM-L6-v2"  // optional
}
```

Response:

```json
{
  "results": [
    {
      "chunk_id": 1,
      "chunk_text": "matching content...",
      "similarity_score": 0.85,
      "filename": "/path/to/file.org",
      "chunk_type": "paragraph",
      "begin_line": 10,
      "end_line": 15
    }
  ],
  "query": "your search query",
  "model_used": "all-MiniLM-L6-v2"
}
```

## Database Schema

SQLite database with 22 tables including:

- `files` - Indexed org files
- `headlines` - Org headings with metadata
- `chunks` - Text chunks for semantic search
- `embeddings` - Vector embeddings (stored as BLOB)
- `links`, `tags`, `properties`, `keywords` - Org metadata
- `images`, `image_embeddings` - CLIP support (future)

## Development

### Running Tests

Python tests:

```bash
cd python
uv run pytest tests/ -v
```

Emacs tests:

```bash
emacs -batch -l tests/org-db-v3-search-test.el -f ert-run-tests-batch-and-exit
```

### Project Structure

```
org-db-v3/
├── python/
│   ├── org_db_server/
│   │   ├── api/           # FastAPI routes
│   │   ├── models/        # Pydantic schemas & DB models
│   │   └── services/      # Business logic
│   └── tests/
├── elisp/
│   ├── org-db-v3.el           # Main package
│   ├── org-db-v3-parse.el     # Org parsing
│   ├── org-db-v3-client.el    # HTTP client
│   ├── org-db-v3-server.el    # Server management
│   └── org-db-v3-search.el    # Search UI
└── tests/                     # Emacs tests
```

## How It Works

### Indexing Pipeline

1. **Emacs parses** org file using `org-element-parse-buffer`
2. **Extracts** headlines, links, keywords, src blocks
3. **Sends JSON** to FastAPI server via async HTTP
4. **Server stores** structured data in SQLite
5. **Generates embeddings** for headline text chunks
6. **Stores vectors** as float32 bytes in database

### Search Pipeline

1. **User enters query** in Emacs minibuffer
2. **Server generates** query embedding
3. **Calculates cosine similarity** with all stored embeddings
4. **Returns top N** results ranked by similarity
5. **Emacs displays** in org-mode buffer with navigation

### Embedding Model

Default: `all-MiniLM-L6-v2` (384 dimensions)
- Fast inference
- Good accuracy for semantic search
- Runs locally (no API calls)

Downloaded automatically on first use (~90MB).

## Advanced

### Custom Embedding Model

```python
# In Python server config
from org_db_server.services.embeddings import get_embedding_service

service = get_embedding_service("sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2")
```

### CLIP Image Search (Future)

CLIP service is implemented but not yet integrated:

```python
from org_db_server.services.clip_service import get_clip_service

clip = get_clip_service()
text_emb = clip.generate_text_embedding("a photo of a cat")
image_emb = clip.generate_image_embedding("cat.jpg")
similarity = clip.similarity(text_emb, image_emb)
```

Requires downloading CLIP model (~600MB) on first use.

## Troubleshooting

### Server won't start

Check if port 8765 is already in use:

```bash
lsof -i :8765
```

Change port in both Python and Emacs config if needed.

### No search results

1. Verify files are indexed: Check server logs
2. Ensure embeddings generated: Look for `embedding_service` in logs
3. Check database: `python -c "import sqlite3; print(sqlite3.connect('org_db.db').execute('SELECT COUNT(*) FROM embeddings').fetchone())"`

### Emacs hangs

- Indexing should be non-blocking via async HTTP
- Check `org-db-v3-index-queue` variable
- Disable with `(org-db-v3-disable)` if needed

### Import errors

Ensure uv environment is activated:

```bash
cd python
uv sync
uv run python -c "import org_db_server"
```

## Performance

- **Indexing**: ~100 headlines/second
- **Search**: <100ms for 1000 documents (local)
- **Embedding generation**: ~50 sentences/second (CPU)
- **Database**: SQLite handles 100k+ chunks efficiently

## Limitations

- Local only (no remote server support yet)
- Text search only (image search planned)
- English-optimized (multilingual models available)
- No fulltext search integrated (FTS5 table exists but unused)

## Roadmap

- [ ] Fulltext search integration
- [ ] Image search via CLIP
- [ ] Transient menu UI
- [ ] Export/import database
- [ ] Multi-user support
- [ ] Encryption for sensitive notes

## License

See LICENSE file.

## Contributing

Contributions welcome! Please:

1. Write tests for new features
2. Follow existing code style
3. Update documentation
4. Use TDD methodology

## Credits

Built with:
- [FastAPI](https://fastapi.tiangolo.com/)
- [sentence-transformers](https://www.sbert.net/)
- [plz.el](https://github.com/alphapapa/plz.el)
- [uv](https://github.com/astral-sh/uv)

Inspired by org-roam, org-ql, and semantic search research.
