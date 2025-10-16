# Multi-Database Architecture

## Overview

Split the monolithic database into three focused databases for better performance and manageability:

1. **Main DB** (`org-db-v3.db`): Metadata, headlines, links, properties, FTS5
2. **Semantic DB** (`org-db-v3-semantic.db`): Chunks + embeddings with vector search
3. **Image DB** (`org-db-v3-images.db`): Images + CLIP embeddings with vector search

## Benefits

- **Smaller individual databases** - easier to manage and backup
- **Faster searches** - vector search only scans relevant DB
- **Independent optimization** - can VACUUM/ANALYZE each separately
- **Clearer separation** - FTS5 vs semantic vs image search
- **Better performance** - reduced I/O per query

## Database Breakdown

### Main Database: `org-db-v3.db`
**Purpose:** Metadata and full-text search
**Size:** ~100-500 MB for 1,300 files
**Contents:**
- `files` - file metadata, MD5 hashes
- `headlines` - org headings with TODO/tags/priority
- `links` - all links with types and paths
- `keywords`, `properties`, `tags` - metadata
- `src_blocks`, `timestamps` - code and dates
- `linked_files` - metadata about linked documents
- `fts_content` - FTS5 full-text search index

**No vector data** - pure metadata and FTS5

### Semantic Database: `org-db-v3-semantic.db`
**Purpose:** Text embeddings and semantic search
**Size:** ~20-50 MB for 1,300 files with 2048-char chunks
**Contents:**
- `chunks` - text chunks with line numbers
- `embeddings` - 384-dim vectors with libsql vector index

**Key feature:** libsql vector search with `vector_top_k()`

### Image Database: `org-db-v3-images.db`
**Purpose:** Image embeddings and CLIP search
**Size:** ~10-30 MB for typical image collection
**Contents:**
- `images` - image paths and metadata
- `image_embeddings` - 512-dim CLIP vectors with libsql vector index

**Key feature:** libsql vector search with `vector_top_k()`

## Database Service Changes

### Old Architecture
```python
class Database:
    def __init__(self, db_path):
        self.conn = libsql.connect(str(db_path))
```

### New Architecture
```python
class Database:
    def __init__(self, main_path, semantic_path, image_path):
        self.main_conn = libsql.connect(str(main_path))
        self.semantic_conn = libsql.connect(str(semantic_path))
        self.image_conn = libsql.connect(str(image_path))
```

## File Organization

```
~/org-db/
├── org-db-v3.db              # Main: 200 MB (metadata + FTS5)
├── org-db-v3-semantic.db     # Semantic: 40 MB (chunks + embeddings)
└── org-db-v3-images.db       # Images: 15 MB (images + CLIP)
-------------------------------------------------------------------
Total: ~255 MB vs 29.5 GB previously!
```

## API Changes (Python only - Emacs unchanged)

### Indexing
```python
# OLD: All in one DB
db.store_chunks(file_id, chunks, embeddings, model)
db.store_images(file_id, images, embeddings, model)

# NEW: Separate DBs with filename reference
db.store_chunks_semantic(filename, chunks, embeddings, model)
db.store_images_clip(filename, images, embeddings, model)
```

### Searching
```python
# OLD: Single DB connection
cursor = db.conn.cursor()
cursor.execute("SELECT ... FROM embeddings ...")

# NEW: Use semantic DB
cursor = db.semantic_conn.cursor()
cursor.execute("SELECT ... FROM embeddings ...")
```

### Stats
```python
# OLD: Single DB stats
stats = {
    'db_size': os.path.getsize(db_path),
    'chunks_count': count_from_chunks_table()
}

# NEW: Multi-DB stats
stats = {
    'main_db_size': os.path.getsize(main_path),
    'semantic_db_size': os.path.getsize(semantic_path),
    'image_db_size': os.path.getsize(image_path),
    'total_size': sum of above
}
```

## Implementation Steps

1. ✅ Update config with 3 database paths
2. ✅ Create schemas for semantic and image DBs
3. ✅ Update main schema (remove chunks/embeddings/images)
4. ⏳ Modify `Database` class to handle 3 connections
5. ⏳ Update `store_chunks()` to write to semantic DB
6. ⏳ Update `store_images()` to write to image DB
7. ⏳ Update search endpoints to query correct DBs
8. ⏳ Update stats endpoint for multi-DB reporting
9. ⏳ Update web interface to show all 3 DBs
10. ⏳ Test end-to-end

## Migration for Users

**Clean start (recommended):**
```bash
# Backup old database
mv ~/org-db/org-db-v3.db ~/org-db/org-db-v3.db.old

# Start server (creates new 3-DB structure)
# Reindex from Emacs
```

**No Emacs changes needed** - API endpoints stay the same!

## Expected Results

For 1,300 org files with aggressive 2048-char chunking:

| Database | Size | Contents |
|----------|------|----------|
| Main | 200 MB | Metadata + FTS5 |
| Semantic | 40 MB | ~6,500 chunks + embeddings |
| Images | 15 MB | ~1,000 images + CLIP |
| **Total** | **255 MB** | vs 29.5 GB before! |

## Performance Improvements

- **Semantic search:** Only scans 40 MB DB (vs 29.5 GB)
- **Image search:** Only scans 15 MB DB (vs 29.5 GB)
- **FTS5 search:** Only scans 200 MB DB (vs 29.5 GB)
- **VACUUM:** Can optimize each DB independently
- **Backup:** Can backup semantic DB separately (most critical)

## Vector Search Confirmation

Both semantic and image DBs use libsql vector indexes:

```sql
-- Semantic DB
CREATE INDEX idx_embeddings_vector
    ON embeddings(libsql_vector_idx(embedding_vector));

-- Image DB
CREATE INDEX idx_image_embeddings_vector
    ON image_embeddings(libsql_vector_idx(embedding_vector));
```

Queries use `vector_top_k()` for fast ANN search.
