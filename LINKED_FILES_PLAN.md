# Linked Files Indexing - Implementation Plan

## Overview

Add support for indexing files linked in org-mode documents (PDF, DOCX, etc.) using docling for conversion to markdown. Chunks from linked files will be stored in the existing vector database, but will always reference the org file and line number where the link exists.

## Design Goals

1. **Reuse existing infrastructure**: Leverage current chunks/embeddings tables
2. **Preserve context**: All search results show the org file + line where link exists
3. **Enable file viewing**: Allow opening the actual linked file from search results
4. **Automatic indexing**: Index linked files when their parent org file is indexed
5. **Incremental updates**: Re-index only when linked file changes (MD5 tracking)

## Database Schema

### New Table: `linked_files`

```sql
CREATE TABLE linked_files (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    org_file_id INTEGER NOT NULL,              -- Parent org file
    org_link_line INTEGER NOT NULL,            -- Line number of link in org file
    file_path TEXT NOT NULL,                   -- Absolute path to linked file
    file_type TEXT NOT NULL,                   -- Extension: pdf, docx, xlsx, etc
    md5 TEXT NOT NULL,                         -- MD5 hash for change detection
    file_size INTEGER,                         -- Size in bytes
    markdown_content TEXT,                     -- Converted markdown (for debugging)
    indexed_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    last_modified TIMESTAMP,                   -- File mtime
    conversion_status TEXT DEFAULT 'pending',  -- pending, success, failed
    conversion_error TEXT,                     -- Error message if failed
    FOREIGN KEY (org_file_id) REFERENCES files(id) ON DELETE CASCADE,
    UNIQUE(org_file_id, file_path)            -- One entry per file per org doc
);

CREATE INDEX idx_linked_files_org ON linked_files(org_file_id);
CREATE INDEX idx_linked_files_path ON linked_files(file_path);
```

### Modified Table: `chunks`

Add optional foreign key to track linked file source:

```sql
ALTER TABLE chunks ADD COLUMN linked_file_id INTEGER;
ALTER TABLE chunks ADD FOREIGN KEY (linked_file_id) REFERENCES linked_files(id) ON DELETE CASCADE;

CREATE INDEX idx_chunks_linked_file ON chunks(linked_file_id);
```

**Important**:
- `filename_id` still points to the org file
- `begin_line` and `end_line` point to the line in the org file where the link exists
- `linked_file_id` is NULL for regular org content, non-NULL for linked file chunks

## Indexing Pipeline

### 1. Link Detection (Emacs)

When parsing org files, extract all file links:

```elisp
(defun org-db-v3-parse-file-links (buffer)
  "Extract all file links from BUFFER.
Returns list of plists with :type :path :line."
  (with-current-buffer buffer
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (when (member (org-element-property :type link) '("file" "attachment"))
          (let* ((path (org-element-property :path link))
                 (begin (org-element-property :begin link))
                 (line (line-number-at-pos begin)))
            (list :type (org-element-property :type link)
                  :path (expand-file-name path)
                  :line line
                  :exists (file-exists-p (expand-file-name path)))))))))
```

### 2. File Type Filtering

Only process files that docling can handle:

```python
# Docling supported formats (as of v2.0, October 2024)
SUPPORTED_EXTENSIONS = {
    # Office documents
    '.pdf',
    '.docx', '.doc',
    '.xlsx', '.xls',
    '.pptx', '.ppt',

    # Web/markup formats
    '.html', '.htm', '.xhtml',
    '.md', '.markdown',
    '.asciidoc', '.adoc',

    # Data formats
    '.csv',

    # Image formats (with OCR)
    '.png', '.jpg', '.jpeg',
    '.tiff', '.tif',
    '.bmp', '.webp',

    # Specialized formats
    '.vtt',  # WebVTT (video subtitles)
    '.xml',  # USPTO XML, JATS XML
    '.json', # Docling JSON
}

# Audio formats (require ASR - Automatic Speech Recognition)
AUDIO_EXTENSIONS = {
    '.wav', '.mp3', '.m4a', '.ogg'
}

def is_indexable_file(file_path: str) -> bool:
    """Check if file type is supported by docling."""
    ext = Path(file_path).suffix.lower()
    return ext in SUPPORTED_EXTENSIONS

def is_audio_file(file_path: str) -> bool:
    """Check if file is audio format (requires ASR)."""
    ext = Path(file_path).suffix.lower()
    return ext in AUDIO_EXTENSIONS
```

### 3. Docling Conversion Service

```python
# python/org_db_server/services/docling_service.py

from docling.document_converter import DocumentConverter
from pathlib import Path
import hashlib

class DoclingService:
    """Convert various document formats to markdown using docling."""

    def __init__(self):
        self.converter = DocumentConverter()

    def convert_to_markdown(self, file_path: str) -> dict:
        """Convert document to markdown.

        Returns:
            {
                'markdown': str,
                'success': bool,
                'error': Optional[str],
                'metadata': dict  # page count, etc
            }
        """
        try:
            result = self.converter.convert(file_path)
            markdown = result.document.export_to_markdown()

            return {
                'markdown': markdown,
                'success': True,
                'error': None,
                'metadata': {
                    'pages': len(result.document.pages) if hasattr(result.document, 'pages') else None
                }
            }
        except Exception as e:
            return {
                'markdown': None,
                'success': False,
                'error': str(e),
                'metadata': {}
            }

    def calculate_md5(self, file_path: str) -> str:
        """Calculate MD5 hash of file."""
        hash_md5 = hashlib.md5()
        with open(file_path, "rb") as f:
            for chunk in iter(lambda: f.read(4096), b""):
                hash_md5.update(chunk)
        return hash_md5.hexdigest()
```

### 4. Chunking Strategy

Linked files will use the same chunking strategy as org content:

```python
def chunk_linked_file_markdown(
    markdown: str,
    org_file_id: int,
    org_link_line: int,
    chunk_size: int = 512,
    overlap: int = 50
) -> List[dict]:
    """Chunk markdown from linked file.

    All chunks reference the org file and link line, not the linked file.
    """
    chunks = []
    sentences = split_into_sentences(markdown)

    current_chunk = []
    current_length = 0

    for sentence in sentences:
        sentence_length = len(sentence)

        if current_length + sentence_length > chunk_size and current_chunk:
            chunks.append({
                'text': ' '.join(current_chunk),
                'filename_id': org_file_id,
                'begin_line': org_link_line,
                'end_line': org_link_line,  # Same line - it's the link location
                'chunk_type': 'linked_file'
            })

            # Overlap: keep last sentence(s)
            overlap_text = ' '.join(current_chunk[-2:])
            current_chunk = [overlap_text, sentence] if len(overlap_text) < overlap else [sentence]
            current_length = len(' '.join(current_chunk))
        else:
            current_chunk.append(sentence)
            current_length += sentence_length

    # Add remaining chunk
    if current_chunk:
        chunks.append({
            'text': ' '.join(current_chunk),
            'filename_id': org_file_id,
            'begin_line': org_link_line,
            'end_line': org_link_line,
            'chunk_type': 'linked_file'
        })

    return chunks
```

### 5. Indexing API Endpoint

```python
@router.post("/api/linked-file")
async def index_linked_file(request: IndexLinkedFileRequest):
    """Index a file linked from an org document.

    Request:
        {
            "org_file_id": 123,
            "org_link_line": 45,
            "file_path": "/path/to/document.pdf"
        }
    """
    # Check if already indexed
    existing = db.get_linked_file(request.org_file_id, request.file_path)

    # Calculate MD5
    md5 = docling_service.calculate_md5(request.file_path)

    # Skip if unchanged
    if existing and existing['md5'] == md5:
        return {"status": "unchanged", "linked_file_id": existing['id']}

    # Convert to markdown
    conversion = docling_service.convert_to_markdown(request.file_path)

    if not conversion['success']:
        # Store failure
        db.store_linked_file_error(
            org_file_id=request.org_file_id,
            org_link_line=request.org_link_line,
            file_path=request.file_path,
            error=conversion['error']
        )
        return {"status": "error", "error": conversion['error']}

    # Store linked file record
    linked_file_id = db.store_linked_file(
        org_file_id=request.org_file_id,
        org_link_line=request.org_link_line,
        file_path=request.file_path,
        file_type=Path(request.file_path).suffix,
        md5=md5,
        file_size=os.path.getsize(request.file_path),
        markdown_content=conversion['markdown'][:10000],  # Store sample
        conversion_status='success'
    )

    # Chunk and embed
    chunks = chunk_linked_file_markdown(
        markdown=conversion['markdown'],
        org_file_id=request.org_file_id,
        org_link_line=request.org_link_line
    )

    # Store chunks with embeddings
    chunk_ids = []
    for chunk in chunks:
        chunk_id = db.store_chunk(
            filename_id=chunk['filename_id'],
            chunk_text=chunk['text'],
            chunk_type=chunk['chunk_type'],
            begin_line=chunk['begin_line'],
            end_line=chunk['end_line'],
            linked_file_id=linked_file_id
        )

        # Generate embedding
        embedding = embedding_service.generate_embedding(chunk['text'])
        db.store_embedding(
            chunk_id=chunk_id,
            embedding_vector=embedding.tobytes(),
            embedding_model='all-MiniLM-L6-v2'
        )

        chunk_ids.append(chunk_id)

    return {
        "status": "success",
        "linked_file_id": linked_file_id,
        "chunks_created": len(chunk_ids)
    }
```

## Emacs Integration

### Automatic Detection

```elisp
(defun org-db-v3-index-file-with-links (filename)
  "Index org file and any linked files."
  (let* ((file-links (org-db-v3-parse-file-links filename))
         (indexable-links (seq-filter
                           (lambda (link)
                             (and (plist-get link :exists)
                                  (org-db-v3-is-indexable-file-p
                                   (plist-get link :path))))
                           file-links)))

    ;; Index main org file first
    (org-db-v3-index-file-async filename)

    ;; Index linked files
    (dolist (link indexable-links)
      (org-db-v3-index-linked-file-async
       :org-filename filename
       :link-line (plist-get link :line)
       :file-path (plist-get link :path)))))

(defun org-db-v3-is-indexable-file-p (file-path)
  "Check if file extension is supported for indexing."
  (member (downcase (file-name-extension file-path))
          '("pdf" "docx" "doc" "pptx" "ppt" "xlsx" "xls" "html" "htm" "md")))
```

### Search Result Display

```elisp
(defun org-db-v3-display-search-results (query response)
  "Display search results with linked file support."
  (let* ((results (alist-get 'results response)))

    (dolist (result results)
      (let* ((chunk-type (alist-get 'chunk_type result))
             (filename (alist-get 'filename result))
             (line (alist-get 'begin_line result))
             (linked-file (alist-get 'linked_file_path result))  ; New field
             (display-text (if (string= chunk-type "linked_file")
                              (format "[%s] %s:%d → %s"
                                      (file-name-nondirectory linked-file)
                                      (file-name-nondirectory filename)
                                      line
                                      (truncate-string-to-width text 60))
                            (format "%s:%d %s"
                                    (file-name-nondirectory filename)
                                    line
                                    (truncate-string-to-width text 60)))))

        ;; Store both org file and linked file in metadata
        (push (list :org-file filename
                    :org-line line
                    :linked-file linked-file
                    :chunk-type chunk-type)
              metadata)))))

(defun org-db-v3-open-search-result (metadata)
  "Open search result - org file or linked file."
  (let ((chunk-type (plist-get metadata :chunk-type)))
    (if (string= chunk-type "linked_file")
        ;; Open linked file
        (let ((linked-file (plist-get metadata :linked-file)))
          (if (y-or-n-p (format "Open linked file %s (n=show link in org)? "
                                (file-name-nondirectory linked-file)))
              (find-file-other-window linked-file)
            ;; Show link location in org file
            (find-file (plist-get metadata :org-file))
            (goto-line (plist-get metadata :org-line))))
      ;; Regular org content
      (find-file (plist-get metadata :org-file))
      (goto-line (plist-get metadata :org-line)))))
```

## Configuration

### Emacs

```elisp
(defcustom org-db-v3-index-linked-files t
  "Whether to automatically index files linked in org documents."
  :type 'boolean
  :group 'org-db-v3)

(defcustom org-db-v3-linked-file-extensions
  '(;; Office documents
    "pdf" "docx" "doc" "pptx" "ppt" "xlsx" "xls"
    ;; Web/markup
    "html" "htm" "xhtml" "md" "markdown" "asciidoc" "adoc"
    ;; Data
    "csv"
    ;; Images (with OCR)
    "png" "jpg" "jpeg" "tiff" "tif" "bmp" "webp"
    ;; Specialized
    "vtt" "xml" "json")
  "File extensions to index when linked from org files.
These formats are supported by docling for conversion to markdown."
  :type '(repeat string)
  :group 'org-db-v3)

(defcustom org-db-v3-index-audio-files nil
  "Whether to index audio files (requires ASR - slow).
When enabled, audio files (.wav, .mp3, etc.) will be transcribed
using Automatic Speech Recognition before indexing."
  :type 'boolean
  :group 'org-db-v3)

(defcustom org-db-v3-index-images t
  "Whether to index image files using OCR.
When enabled, images linked in org files will be processed with
OCR to extract text content. Requires docling OCR support."
  :type 'boolean
  :group 'org-db-v3)

(defcustom org-db-v3-max-linked-file-size (* 50 1024 1024)
  "Maximum size (in bytes) of linked files to index.
Files larger than this will be skipped. Default: 50MB."
  :type 'integer
  :group 'org-db-v3)
```

### Python

```python
# config.py
class Settings:
    # Existing settings...

    # Linked files
    index_linked_files: bool = True
    max_linked_file_size: int = 50 * 1024 * 1024  # 50MB
    docling_batch_size: int = 5  # Process N files at a time
```

## UI Enhancements

### Statistics Display

Add to web interface:

```html
<div class="stat-card">
    <h3>Linked Files</h3>
    <div class="value">${stats.linked_files_count}</div>
</div>
```

### Management Commands

```elisp
(defun org-db-v3-list-linked-files ()
  "Show all indexed linked files."
  (interactive)
  ;; Call /api/linked-files endpoint
  ;; Display in buffer with:
  ;; - File path
  ;; - Source org file + line
  ;; - Chunks count
  ;; - Last indexed
  )

(defun org-db-v3-reindex-linked-file ()
  "Force re-index of linked file at point."
  (interactive)
  ;; If on a file link, reindex that specific file
  )
```

## Migration

Database migration to add new structures:

```python
# python/org_db_server/migrations/add_linked_files.py

def migrate_up(conn):
    """Add linked files support."""
    cursor = conn.cursor()

    # Create linked_files table
    cursor.execute("""
        CREATE TABLE IF NOT EXISTS linked_files (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            org_file_id INTEGER NOT NULL,
            org_link_line INTEGER NOT NULL,
            file_path TEXT NOT NULL,
            file_type TEXT NOT NULL,
            md5 TEXT NOT NULL,
            file_size INTEGER,
            markdown_content TEXT,
            indexed_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            last_modified TIMESTAMP,
            conversion_status TEXT DEFAULT 'pending',
            conversion_error TEXT,
            FOREIGN KEY (org_file_id) REFERENCES files(id) ON DELETE CASCADE,
            UNIQUE(org_file_id, file_path)
        )
    """)

    cursor.execute("CREATE INDEX idx_linked_files_org ON linked_files(org_file_id)")
    cursor.execute("CREATE INDEX idx_linked_files_path ON linked_files(file_path)")

    # Add column to chunks
    cursor.execute("ALTER TABLE chunks ADD COLUMN linked_file_id INTEGER")
    cursor.execute("CREATE INDEX idx_chunks_linked_file ON chunks(linked_file_id)")

    conn.commit()

def migrate_down(conn):
    """Remove linked files support."""
    cursor = conn.cursor()
    cursor.execute("DROP TABLE IF EXISTS linked_files")
    # Note: SQLite doesn't support DROP COLUMN easily
    conn.commit()
```

## Testing Strategy

### Unit Tests

1. Docling conversion for each supported file type
2. Chunking linked file markdown
3. MD5 change detection
4. Database operations (store/retrieve linked files)

### Integration Tests

1. Index org file with PDF link
2. Search returns results from linked file
3. Results show correct org file + line
4. Opening result opens linked file
5. Re-indexing skips unchanged files

### Test Data

Create test fixtures:
- sample.org (with links to test files)
- sample.pdf
- sample.docx
- sample_large.pdf (> max size)

## Implementation Order

1. ✅ Database schema and migration
2. ✅ Docling service with error handling
3. ✅ Linked file indexing endpoint
4. ✅ Emacs link parser
5. ✅ Integration with main indexing pipeline
6. ✅ Search result modifications
7. ✅ UI updates (stats, management)
8. ✅ Configuration options
9. ✅ Testing and validation
10. ✅ Documentation

## Performance Considerations

1. **Conversion Overhead**: Docling conversion can be slow
   - Process in background/queue
   - Skip re-conversion based on MD5
   - Add timeout for large files

2. **Storage**: Markdown content can be large
   - Only store first 10KB for debugging
   - Rely on chunks for search

3. **Indexing Time**: Don't block org file indexing
   - Index linked files asynchronously after org file
   - Show progress in UI

4. **Search Performance**: No impact
   - Linked file chunks use same indexes
   - Same vector search performance

## Future Enhancements

1. **Image extraction**: Extract and index images from PDFs/DOCX
2. **OCR support**: For scanned PDFs
3. **Incremental updates**: Re-index only changed pages
4. **File watching**: Auto-reindex when linked files change
5. **Link validation**: Warn about broken links
6. **Batch operations**: Reindex all linked files command

## Questions to Resolve

1. Should we support relative paths in links or only absolute?
2. What to do with links to non-existent files?
3. Should we show a diff when linked file changes?
4. How to handle very large files (100MB+)?
5. Should we support links to remote URLs (http/https)?
