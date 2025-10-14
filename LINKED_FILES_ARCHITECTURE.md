# Linked Files Architecture

## Data Flow

```
┌─────────────────────────────────────────────────────────────────┐
│                         Org File (inbox.org)                     │
│                                                                   │
│  Line 45: [[file:~/Documents/report.pdf][Q3 Report]]           │
│  Line 67: [[file:~/Papers/research.docx][Research Paper]]      │
└─────────────────────────────────────────────────────────────────┘
                            │
                            │ 1. Parse links
                            ▼
┌─────────────────────────────────────────────────────────────────┐
│                    Emacs: org-db-v3-parse                        │
│  - Extract file links with line numbers                          │
│  - Filter by extension (.pdf, .docx, etc)                       │
│  - Check file exists                                             │
└─────────────────────────────────────────────────────────────────┘
                            │
                            │ 2. Send to server
                            ▼
┌─────────────────────────────────────────────────────────────────┐
│                 Python: Docling Service                          │
│  - Calculate MD5 hash                                            │
│  - Convert PDF/DOCX → Markdown                                   │
│  - Handle errors gracefully                                      │
└─────────────────────────────────────────────────────────────────┘
                            │
                            │ 3. Chunk markdown
                            ▼
┌─────────────────────────────────────────────────────────────────┐
│                     Chunking Service                             │
│  - Split markdown into ~512 char chunks                          │
│  - Add overlap for context                                       │
│  - Tag with chunk_type = 'linked_file'                          │
└─────────────────────────────────────────────────────────────────┘
                            │
                            │ 4. Store chunks
                            ▼
┌─────────────────────────────────────────────────────────────────┐
│                        Database                                  │
│                                                                   │
│  linked_files table:                                             │
│  ┌─────┬──────────┬───────┬────────────┬────────────┐          │
│  │ id  │ org_file │ line  │ file_path  │ md5        │          │
│  ├─────┼──────────┼───────┼────────────┼────────────┤          │
│  │ 1   │ inbox.org│  45   │ report.pdf │ a3b2c1...  │          │
│  │ 2   │ inbox.org│  67   │ research...│ f5e4d3...  │          │
│  └─────┴──────────┴───────┴────────────┴────────────┘          │
│                                                                   │
│  chunks table:                                                   │
│  ┌─────┬──────────┬──────┬──────┬────────────┬──────────────┐  │
│  │ id  │ filename │ line │ text │ chunk_type │ linked_file  │  │
│  ├─────┼──────────┼──────┼──────┼────────────┼──────────────┤  │
│  │ 101 │ inbox.org│  45  │ Q3...│ linked_file│      1       │  │
│  │ 102 │ inbox.org│  45  │ Rev..│ linked_file│      1       │  │
│  │ 103 │ inbox.org│  67  │ The..│ linked_file│      2       │  │
│  └─────┴──────────┴──────┴──────┴────────────┴──────────────┘  │
│         ▲                    ▲                                   │
│         │                    │                                   │
│         │                    └─ Points to org file + link line   │
│         │                       NOT to the PDF/DOCX             │
│         │                                                         │
│         └─ Embedded via embeddings table                         │
│            (reuses existing infrastructure)                      │
└─────────────────────────────────────────────────────────────────┘
                            │
                            │ 5. Search
                            ▼
┌─────────────────────────────────────────────────────────────────┐
│                      Semantic Search                             │
│  Query: "Q3 revenue trends"                                      │
│  Results:                                                        │
│  1. [report.pdf] inbox.org:45 "Q3 revenue increased by..."     │
│  2. inbox.org:120 "Planning for Q4 revenue targets"            │
└─────────────────────────────────────────────────────────────────┘
                            │
                            │ 6. User selection
                            ▼
┌─────────────────────────────────────────────────────────────────┐
│                       Emacs: Jump to result                      │
│                                                                   │
│  Option A: Open ~/Documents/report.pdf (the linked file)        │
│  Option B: Jump to inbox.org:45 (the link location)            │
└─────────────────────────────────────────────────────────────────┘
```

## Key Design Decisions

### 1. Chunks Point to Org Files, Not Linked Files

**Why**: Preserve context and enable navigation back to the source document

```
❌ WRONG APPROACH:
chunks.filename = "report.pdf"
chunks.line = 15  ← Line in PDF? No clear meaning!

✅ CORRECT APPROACH:
chunks.filename = "inbox.org"
chunks.line = 45  ← Line of the link in org file
chunks.linked_file_id = 1  ← Reference to actual file
```

### 2. Reuse Existing Tables

**Why**: Leverage existing vector search infrastructure

- No changes to `embeddings` table
- No changes to search queries
- Seamless integration with current semantic search
- Just add one nullable FK to `chunks`

### 3. Separate Linked Files Table

**Why**: Track linked file metadata without polluting files table

- `files` table = org files only (existing behavior)
- `linked_files` table = PDFs, DOCX, etc (new)
- Clear separation of concerns
- Easy to add linked-file-specific fields (conversion_status, etc)

### 4. MD5 Change Detection

**Why**: Avoid re-converting unchanged files

```python
# Fast path: File unchanged
if existing_md5 == current_md5:
    return {"status": "unchanged"}

# Slow path: File changed, re-convert
docling.convert(file_path)
```

### 5. Chunk Type Tag

**Why**: Enable special handling in search results

```python
if chunk['chunk_type'] == 'linked_file':
    # Show file icon
    # Offer to open linked file
    # Display differently in UI
```

## Search Result Format

### Regular Org Content

```
0.876 | "TODO Review the quarterly report..."
        inbox.org:120
        [Opens inbox.org at line 120]
```

### Linked File Content

```
0.892 | [📄 report.pdf] "Q3 revenue increased by 23%..."
        inbox.org:45 → report.pdf
        [Choice: Open PDF or jump to link]
```

## Example Queries

### Find content from linked PDFs

```elisp
(org-db-v3-semantic-search "Q3 financial results")
→ Returns chunks from report.pdf
→ Each chunk shows: inbox.org:45 (where link exists)
→ Can open: report.pdf or inbox.org
```

### Find org files that link to PDFs about "revenue"

```elisp
(org-db-v3-semantic-search "revenue trends")
→ Returns mixed results:
  - Direct org content matching "revenue"
  - Content from linked PDFs matching "revenue"
→ User can distinguish by [📄] icon or chunk_type
```

## Database Queries

### Get all linked files for an org file

```sql
SELECT lf.file_path, lf.org_link_line, COUNT(c.id) as chunk_count
FROM linked_files lf
JOIN chunks c ON c.linked_file_id = lf.id
WHERE lf.org_file_id = ?
GROUP BY lf.id;
```

### Get chunks from a specific linked file

```sql
SELECT c.chunk_text, c.begin_line, f.filename
FROM chunks c
JOIN files f ON c.filename_id = f.id
WHERE c.linked_file_id = ?;
```

### Search across all content (org + linked)

```sql
-- No change needed! Works automatically
SELECT c.chunk_text, c.begin_line, f.filename,
       lf.file_path as linked_file
FROM chunks c
JOIN files f ON c.filename_id = f.id
LEFT JOIN linked_files lf ON c.linked_file_id = lf.id
WHERE c.id IN (SELECT chunk_id FROM embeddings WHERE ...);
```

## Error Handling

### Conversion Failures

```python
# Store failure in database
linked_files.conversion_status = 'failed'
linked_files.conversion_error = 'Encrypted PDF'

# Don't block org file indexing
# User can see failed conversions in UI
# Can retry manually later
```

### Missing Files

```python
# During indexing
if not os.path.exists(file_path):
    # Skip, log warning
    # Don't create linked_files entry

# During re-indexing
if not os.path.exists(file_path):
    # Delete linked_files entry
    # CASCADE deletes chunks
```

### Large Files

```python
if os.path.getsize(file_path) > MAX_SIZE:
    linked_files.conversion_status = 'skipped'
    linked_files.conversion_error = f'File too large: {size}MB'
    # Don't process, notify user
```

## Configuration Matrix

| Setting | Default | Description |
|---------|---------|-------------|
| `org-db-v3-index-linked-files` | `t` | Enable linked file indexing |
| `org-db-v3-linked-file-extensions` | `["pdf", "docx", ...]` | File types to index |
| `org-db-v3-max-linked-file-size` | `50MB` | Skip files larger than this |
| `org-db-v3-linked-file-open-action` | `ask` | `ask`, `file`, or `link` |

## Migration Path

1. Run migration to add `linked_files` table
2. Add `linked_file_id` column to `chunks`
3. Existing chunks have `linked_file_id = NULL` (regular org content)
4. New indexing automatically populates linked files
5. No data loss, backward compatible

## Performance Impact

- **Indexing**: Slower (docling conversion overhead)
  - Mitigated by: MD5 caching, background processing
- **Search**: No impact (same vector search)
- **Storage**: More chunks (but same structure)
  - Typical PDF: ~50 chunks = ~2KB
- **Database size**: +10-20% for linked_files table

## Next Steps

1. Add docling to dependencies
2. Create `docling_service.py`
3. Add database migration
4. Implement `/api/linked-file` endpoint
5. Update Emacs parser to extract file links
6. Modify search UI to show linked files
7. Add tests
8. Update documentation
