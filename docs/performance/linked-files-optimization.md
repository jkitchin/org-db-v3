# Linked Files Optimization Guide

## Problem Summary

**Your 29.5 GB database bloat was caused by linked files creating too many chunks:**
- 5,164 linked files (PDFs, DOCX, PPTX, etc.)
- 360,013 total chunks (~70 chunks per linked file average)
- Using paragraph chunking = every paragraph became a separate embedding

**Result:** Database grew from expected ~500 MB to 29.5 GB (60x bloat!)

---

## Solutions Implemented

### 1. **Fixed-Size Chunking (80-90% reduction)**

**Before:** `chunk_text(markdown, method="paragraph")`
- 50-page PDF = 200 paragraphs = 200 chunks

**After:** `chunk_text(markdown, method="fixed", chunk_size=2048, chunk_overlap=200)`
- 50-page PDF = ~10-25 chunks (80-90% fewer!)

**Configuration:** Edit `config.py`
```python
linked_file_chunk_size: int = 2048  # Characters per chunk
linked_file_chunk_overlap: int = 200  # Overlap for context
```

### 2. **File Size Limits**

Skip huge files that would create thousands of chunks:

```python
max_linked_file_size_mb: int = 20  # Skip files > 20MB
```

**Examples:**
- 20 MB = typical ~100-page PDF
- 50 MB = presentation with many images
- 100 MB = large academic paper with figures

Set to `0` to disable limit.

### 3. **Per-File Chunk Limits**

Hard cap on chunks per file:

```python
max_linked_file_chunks: int = 50  # Max 50 chunks per file
```

Even if chunking would create 200 chunks, only first 50 are indexed.

Set to `0` to disable limit.

### 4. **Disable Linked Files**

If you don't need to search linked file content:

```python
enable_linked_files: bool = False
```

Or via environment variable:
```bash
export ORG_DB_ENABLE_LINKED_FILES=false
```

---

## Configuration Options

Edit `/Users/jkitchin/Dropbox/emacs/scimax/org-db-v3/python/org_db_server/config.py`:

```python
class Settings(BaseSettings):
    # Feature flags
    enable_linked_files: bool = True  # Master switch

    # Linked file limits (to prevent database bloat)
    max_linked_file_size_mb: int = 20        # Skip large files (0 = no limit)
    max_linked_file_chunks: int = 50         # Max chunks per file (0 = no limit)
    linked_file_chunk_size: int = 2048       # Larger = fewer chunks
    linked_file_chunk_overlap: int = 200     # Context overlap
```

Or use environment variables:
```bash
export ORG_DB_ENABLE_LINKED_FILES=true
export ORG_DB_MAX_LINKED_FILE_SIZE_MB=20
export ORG_DB_MAX_LINKED_FILE_CHUNKS=50
export ORG_DB_LINKED_FILE_CHUNK_SIZE=2048
export ORG_DB_LINKED_FILE_CHUNK_OVERLAP=200
```

---

## Recommended Configurations

### **Conservative (Minimal Database Size)**
```python
enable_linked_files: bool = False  # Or only enable for specific files
```
**Expected database size:** ~500 MB for 1,300 files

### **Balanced (Recommended)**
```python
enable_linked_files: bool = True
max_linked_file_size_mb: int = 20      # Skip huge files
max_linked_file_chunks: int = 50       # Hard cap per file
linked_file_chunk_size: int = 2048     # 4x larger than default
```
**Expected database size:** ~2-5 GB for 1,300 files + 5,000 PDFs

### **Aggressive (Search Everything)**
```python
enable_linked_files: bool = True
max_linked_file_size_mb: int = 0       # No size limit
max_linked_file_chunks: int = 100      # Allow more chunks
linked_file_chunk_size: int = 1536     # Smaller chunks = better precision
```
**Expected database size:** ~5-15 GB for 1,300 files + 5,000 PDFs

---

## Expected Chunk Counts

With **fixed chunking at 2048 chars** and **50 chunk limit**:

| Document Type | Pages | Old (paragraph) | New (fixed) | Reduction |
|---------------|-------|-----------------|-------------|-----------|
| Small PDF     | 10    | 40 chunks       | 5 chunks    | 87%       |
| Medium PDF    | 50    | 200 chunks      | 25 chunks   | 87%       |
| Large PDF     | 200   | 800 chunks      | 50 chunks*  | 94%       |
| DOCX          | 20    | 80 chunks       | 10 chunks   | 87%       |
| PPTX          | 30    | 60 chunks       | 15 chunks   | 75%       |

\* *Capped by max_linked_file_chunks=50*

**Overall:** ~5,000 linked files would create:
- **Before:** ~350,000 chunks (what you had!)
- **After:** ~50,000 chunks (85% reduction)

---

## Testing After Rebuild

1. **Rebuild database:**
   ```bash
   rm ~/Dropbox/emacs/cache/org-db-v3/org-db-v3.db
   # Restart server, then reindex from Emacs
   ```

2. **Monitor size during indexing:**
   ```bash
   watch -n 5 'du -h ~/Dropbox/emacs/cache/org-db-v3/org-db-v3.db'
   ```

3. **Check stats after indexing:**
   ```bash
   curl -s http://127.0.0.1:8765/api/stats/ | jq
   ```

4. **Calculate chunks per file:**
   ```bash
   # Should be < 50 average (with limits)
   python3 -c "
   import requests
   stats = requests.get('http://127.0.0.1:8765/api/stats/').json()
   chunks_per_file = stats['chunks_count'] / stats['files_count']
   print(f'Chunks per file: {chunks_per_file:.1f}')
   "
   ```

---

## Fine-Tuning

### **If database is still too large:**

1. Reduce chunk size limit:
   ```python
   max_linked_file_chunks: int = 25  # Even fewer chunks
   ```

2. Reduce file size limit:
   ```python
   max_linked_file_size_mb: int = 10  # Skip larger files
   ```

3. Increase chunk size:
   ```python
   linked_file_chunk_size: int = 4096  # Bigger chunks = fewer of them
   ```

4. Disable specific file types by modifying the converter

### **If search quality is poor:**

1. Increase chunk size for more context:
   ```python
   linked_file_chunk_size: int = 3072
   linked_file_chunk_overlap: int = 300
   ```

2. Increase chunk limit:
   ```python
   max_linked_file_chunks: int = 75  # More coverage
   ```

3. Re-enable paragraph chunking for important files only

---

## Trade-offs

| Setting | Pros | Cons |
|---------|------|------|
| **Large chunks (2048+)** | Fewer embeddings, smaller DB, faster search | Less precise matching |
| **Small chunks (512)** | More precise matches | More embeddings, larger DB |
| **Chunk limits** | Predictable DB size | May miss content at end of large files |
| **File size limits** | Skip problematic files | Can't search content of large docs |
| **Disabled linked files** | Minimal DB size | Can't search linked content |

---

## Migration Path

### **Phase 1: Start Clean (Recommended)**
1. Set conservative limits (see above)
2. Rebuild database
3. Monitor size and performance
4. Adjust if needed

### **Phase 2: Gradual Enable**
If you disabled linked files:
1. Enable for one directory/project first
2. Check database size impact
3. Adjust chunk settings
4. Expand to more files

### **Phase 3: Selective Indexing**
In future, add ability to:
- Skip certain file extensions
- Index only recent files
- Index by directory pattern
- Manually select important PDFs

---

## Monitoring Commands

```bash
# Database size
du -h ~/Dropbox/emacs/cache/org-db-v3/org-db-v3.db

# Detailed stats
curl -s http://127.0.0.1:8765/api/stats/ | jq

# Chunks per file ratio
curl -s http://127.0.0.1:8765/api/stats/ | jq '.chunks_count / .files_count'

# Linked files vs org files
curl -s http://127.0.0.1:8765/api/stats/ | jq '{org: .org_files_count, linked: .linked_files_count}'

# Check server logs for skipped files
grep -i "skipping\|limiting" /tmp/org-db-server.log | tail -20
```

---

## Summary

**Before optimization:**
- 360,013 chunks from 5,164 linked files
- 29.5 GB database
- Server crashes on search

**After optimization:**
- ~50,000 chunks (85% reduction)
- ~2-5 GB database (80-90% reduction)
- Fast, reliable searches

**Key changes:**
1. ✅ Fixed-size chunking instead of paragraph chunking
2. ✅ 50 chunk limit per file
3. ✅ 20 MB file size limit
4. ✅ Configurable via environment variables
