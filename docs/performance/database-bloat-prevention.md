# Preventing Database Bloat in org-db-v3

## What Caused the 29.5 GB Bloat?

Your database was **29.5 GB for only 1,301 org files** - that's ~22 MB per file!

### Root Causes:

1. **Linked Files Explosion** (MAJOR)
   - Stats showed: 5,164 linked files (PDFs, DOCX, PPTX, etc.)
   - These generated ~360,013 chunks total
   - Each PDF can create 50-200 chunks depending on size
   - **Solution**: Be selective about which linked files to index

2. **FTS5 Full-Text Index**
   - Stores entire file content for full-text search
   - FTS5 index overhead is 3-4x the raw text size
   - For large files, this adds up quickly
   - **Solution**: Already implemented, but watch for large files

3. **Vector Index Overhead**
   - libsql vector indexes are 2-3x the size of raw embeddings
   - 360K embeddings × 384 floats × 4 bytes = 554 MB raw
   - Vector indexes: ~1-1.5 GB total
   - **Solution**: This is necessary for fast search, but be aware

4. **No VACUUM**
   - SQLite doesn't automatically reclaim deleted space
   - Re-indexing files leaves old data behind
   - **Solution**: Run VACUUM periodically

## Recommendations Going Forward:

### 1. Selective Linked File Indexing

**Option A: Skip linked files entirely**
```elisp
;; In your org-db configuration
(setq org-db-v3-index-linked-files nil)  ;; if this variable exists
```

**Option B: Limit by file type**
Only index PDFs, skip DOCX/PPTX/etc:
- Modify indexing code to check file extension
- Skip conversion for file types you don't need to search

**Option C: Limit by file size**
Skip huge PDFs that create thousands of chunks:
```python
# In document_converter.py or indexing.py
MAX_LINKED_FILE_SIZE = 10 * 1024 * 1024  # 10 MB
if file_size > MAX_LINKED_FILE_SIZE:
    skip indexing
```

### 2. Monitor Database Size

Add this to your workflow:

```bash
# Check database size regularly
du -h ~/Dropbox/emacs/cache/org-db-v3/org-db-v3.db

# Check stats
curl -s http://127.0.0.1:8765/api/stats/ | jq
```

**Warning signs:**
- Database > 5 GB for < 2000 files
- More than 100 chunks per file on average
- Linked files > 5x org files

### 3. Periodic Maintenance

**Monthly:**
```bash
# Optimize query planner
curl -X POST http://127.0.0.1:8765/api/optimize

# Check size
du -h ~/Dropbox/emacs/cache/org-db-v3/org-db-v3.db
```

**Quarterly or when database is slow:**
```bash
# Rebuild from scratch
rm ~/Dropbox/emacs/cache/org-db-v3/org-db-v3.db
# Then reindex from Emacs: M-x org-db-v3-index-all
```

**After large deletions/changes:**
```bash
# Reclaim space (requires server to be stopped)
sqlite3 ~/Dropbox/emacs/cache/org-db-v3/org-db-v3.db "VACUUM"
```

### 4. Chunking Strategy

Current chunking may be too aggressive. Check:
- How many chunks per file are being created?
- Are tiny chunks being created?

**To investigate:**
```bash
# See chunk distribution
curl -s http://127.0.0.1:8765/api/stats/ | jq '.chunks_count, .files_count'
# Should be < 300 chunks per file on average
```

### 5. Reasonable Size Expectations

**For 1,300 org files:**
- **Minimal** (no linked files): 200-500 MB
- **Typical** (some linked files): 500 MB - 2 GB
- **Heavy** (many PDFs indexed): 2-5 GB
- **BLOATED** (your case): > 10 GB 🚩

**For 10,000 org files:**
- **Minimal**: 1-3 GB
- **Typical**: 3-10 GB
- **Heavy**: 10-30 GB

## Quick Fixes for Next Rebuild:

1. **Skip linked files during initial index** to get a baseline
2. **Monitor size** as you add files
3. **Run ANALYZE** after initial index (makes searches fast)
4. **If you need linked files**, add them selectively:
   - Only important PDFs
   - Skip large presentation files
   - Skip temporary/generated documents

## Scripts to Help:

```bash
# Run after rebuild to optimize
python3 optimize_database.py

# Monitor ongoing performance
python3 debug_search_performance.py

# Check what's taking space (requires sqlite3)
sqlite3 ~/Dropbox/emacs/cache/org-db-v3/org-db-v3.db "
SELECT
    name,
    ROUND(SUM(pgsize)/1024.0/1024.0, 2) as size_mb
FROM dbstat
GROUP BY name
ORDER BY size_mb DESC
LIMIT 20;"
```
