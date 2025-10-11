# org-db v3 Implementation Plan

> **For Claude:** Use `${CLAUDE_PLUGIN_ROOT}/skills/collaboration/executing-plans/SKILL.md` to implement this plan task-by-task.

**Goal:** Build a hybrid Emacs/Python system for indexing and searching org-mode files with semantic search, full-text search, and CLIP-based image search capabilities.

**Architecture:** Emacs parses org files using org-element and sends structured JSON to a FastAPI server. The server handles storage (libsql), embedding generation (sentence-transformers), CLIP image encoding, and vector similarity search. Emacs remains responsive via async HTTP requests and Server-Sent Events for progress updates.

**Tech Stack:** Emacs Lisp, Python 3.10+, FastAPI, libsql, sentence-transformers, CLIP, plz.el (async HTTP), transient.el (UI), uv (Python tooling)

---

## Phase 1: Foundation & Project Setup

### Task 1.1: Create Project Structure

**Files:**
- Create: `python/pyproject.toml`
- Create: `python/org_db_server/__init__.py`
- Create: `python/org_db_server/main.py`
- Create: `python/org_db_server/config.py`
- Create: `elisp/org-db-v3.el`
- Create: `tests/fixtures/sample.org`
- Create: `scripts/setup.sh`
- Create: `.gitignore`

**Step 1: Create Python project configuration**

Create `python/pyproject.toml`:
```toml
[project]
name = "org-db-server"
version = "0.1.0"
description = "FastAPI server for org-db v3"
requires-python = ">=3.10"
dependencies = [
    "fastapi>=0.104.0",
    "uvicorn[standard]>=0.24.0",
    "libsql-experimental>=0.1.0",
    "sentence-transformers>=2.2.0",
    "transformers>=4.35.0",
    "torch>=2.1.0",
    "pillow>=10.0.0",
    "numpy>=1.24.0",
    "pydantic>=2.5.0",
    "python-multipart>=0.0.6",
    "sse-starlette>=1.8.2",
]

[project.optional-dependencies]
dev = [
    "pytest>=7.4.0",
    "pytest-asyncio>=0.21.0",
    "httpx>=0.25.0",
]

[build-system]
requires = ["hatchling"]
build-backend = "hatchling.build"

[tool.uv]
dev-dependencies = [
    "pytest>=7.4.0",
    "pytest-asyncio>=0.21.0",
    "httpx>=0.25.0",
]
```

**Step 2: Create basic FastAPI server**

Create `python/org_db_server/main.py`:
```python
"""FastAPI server for org-db v3."""
from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware

app = FastAPI(title="org-db Server", version="0.1.0")

# Allow Emacs to connect from localhost
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

@app.get("/health")
async def health_check():
    """Health check endpoint."""
    return {"status": "ok", "version": "0.1.0"}

@app.get("/")
async def root():
    """Root endpoint."""
    return {"message": "org-db v3 server running"}
```

**Step 3: Create configuration module**

Create `python/org_db_server/config.py`:
```python
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
    default_clip_model: str = "openai/clip-vit-base-patch32"

    # Indexing
    chunk_size: int = 512
    chunk_overlap: int = 50

    class Config:
        env_prefix = "ORG_DB_"

settings = Settings()

# Ensure database directory exists
settings.db_path.parent.mkdir(parents=True, exist_ok=True)
```

**Step 4: Create basic Emacs package**

Create `elisp/org-db-v3.el`:
```elisp
;;; org-db-v3.el --- Org database v3 with semantic search -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Your Name
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (transient "0.4.0") (plz "0.7") (compat "29.1"))
;; Keywords: org, database, search
;; URL: https://github.com/yourusername/org-db-v3

;;; Commentary:

;; org-db v3 provides semantic search, full-text search, and image search
;; capabilities for org-mode files using a Python FastAPI backend.

;;; Code:

(require 'org)
(require 'plz)
(require 'transient)

(defgroup org-db-v3 nil
  "Org database v3 with semantic search."
  :group 'org
  :prefix "org-db-v3-")

(defcustom org-db-v3-server-host "127.0.0.1"
  "Host for org-db server."
  :type 'string
  :group 'org-db-v3)

(defcustom org-db-v3-server-port 8765
  "Port for org-db server."
  :type 'integer
  :group 'org-db-v3)

(defcustom org-db-v3-auto-start-server t
  "Whether to auto-start the server if not running."
  :type 'boolean
  :group 'org-db-v3)

(defvar org-db-v3-server-process nil
  "Process running the org-db server.")

(defun org-db-v3-server-url ()
  "Return the base URL for the org-db server."
  (format "http://%s:%d" org-db-v3-server-host org-db-v3-server-port))

(defun org-db-v3-server-running-p ()
  "Check if the org-db server is running."
  (condition-case nil
      (plz 'get (concat (org-db-v3-server-url) "/health")
        :as #'json-read
        :then (lambda (response) t)
        :else (lambda (error) nil))
    (error nil)))

(provide 'org-db-v3)
;;; org-db-v3.el ends here
```

**Step 5: Create test fixture**

Create `tests/fixtures/sample.org`:
```org
#+TITLE: Sample Org File
#+AUTHOR: Test Author
#+DATE: 2025-10-11

* TODO First Heading
:PROPERTIES:
:ID: test-id-001
:CUSTOM: value
:END:

This is a paragraph with some content about machine learning and neural networks.

** Second Level Heading

Some more content here with a [[https://example.com][link]].

* DONE Completed Task
CLOSED: [2025-10-11 Sat 10:00]
:PROPERTIES:
:EMAIL: test@example.com
:END:

Final content with #hashtag and @mention.
```

**Step 6: Create setup script**

Create `scripts/setup.sh`:
```bash
#!/bin/bash
set -e

echo "Setting up org-db v3 development environment..."

# Check Python version
python3 --version | grep -q "Python 3.1[0-9]" || {
    echo "Error: Python 3.10+ required"
    exit 1
}

# Install uv if not available
if ! command -v uv &> /dev/null; then
    echo "Installing uv..."
    curl -LsSf https://astral.sh/uv/install.sh | sh
fi

# Install Python dependencies
cd python
uv sync

echo "Setup complete!"
echo "To start the server: cd python && uv run uvicorn org_db_server.main:app --reload --port 8765"
```

**Step 7: Create .gitignore**

Create `.gitignore`:
```
# Python
__pycache__/
*.py[cod]
*$py.class
.venv/
venv/
*.egg-info/
dist/
build/
.pytest_cache/

# Emacs
*.elc
*~
\#*\#
.\#*

# Database
*.db
*.db-shm
*.db-wal

# IDE
.vscode/
.idea/

# OS
.DS_Store
Thumbs.db
```

**Step 8: Make setup script executable and test**

Run:
```bash
chmod +x scripts/setup.sh
./scripts/setup.sh
```

Expected: uv installs dependencies successfully

**Step 9: Test server starts**

Run:
```bash
cd python
uv run uvicorn org_db_server.main:app --reload --port 8765
```

Expected: Server starts on http://127.0.0.1:8765

Test in another terminal:
```bash
curl http://127.0.0.1:8765/health
```

Expected: `{"status":"ok","version":"0.1.0"}`

**Step 10: Commit foundation**

```bash
git add .
git commit -m "feat: initial project structure and basic server"
```

---

## Phase 2: Database Layer

### Task 2.1: LibSQL Schema Implementation

**Files:**
- Create: `python/org_db_server/models/db_models.py`
- Create: `python/org_db_server/services/database.py`
- Create: `tests/test_database.py`

**Step 1: Write test for database initialization**

Create `tests/test_database.py`:
```python
"""Tests for database operations."""
import pytest
from pathlib import Path
from org_db_server.services.database import Database

@pytest.fixture
def temp_db(tmp_path):
    """Create a temporary database for testing."""
    db_path = tmp_path / "test.db"
    db = Database(db_path)
    yield db
    db.close()

def test_database_initialization(temp_db):
    """Test that database initializes with correct schema."""
    # Check that files table exists
    cursor = temp_db.conn.cursor()
    cursor.execute("SELECT name FROM sqlite_master WHERE type='table' AND name='files'")
    assert cursor.fetchone() is not None

    # Check that headlines table exists
    cursor.execute("SELECT name FROM sqlite_master WHERE type='table' AND name='headlines'")
    assert cursor.fetchone() is not None
```

**Step 2: Run test to verify it fails**

Run: `cd python && uv run pytest tests/test_database.py::test_database_initialization -v`

Expected: FAIL with "No module named 'org_db_server.services.database'"

**Step 3: Create database models**

Create `python/org_db_server/models/db_models.py`:
```python
"""Database schema definitions."""

# SQL schema for org-db v3
SCHEMA = """
-- Core files table
CREATE TABLE IF NOT EXISTS files (
    rowid INTEGER PRIMARY KEY,
    filename TEXT UNIQUE NOT NULL,
    md5 TEXT NOT NULL,
    last_updated TEXT NOT NULL,
    file_size INTEGER,
    indexed_at TEXT
);

CREATE INDEX IF NOT EXISTS idx_files_filename ON files(filename);
CREATE INDEX IF NOT EXISTS idx_files_md5 ON files(md5);

-- Headlines table
CREATE TABLE IF NOT EXISTS headlines (
    rowid INTEGER PRIMARY KEY,
    filename_id INTEGER NOT NULL,
    title TEXT NOT NULL,
    level INTEGER NOT NULL,
    todo_keyword TEXT,
    todo_type TEXT,
    archivedp INTEGER,
    commentedp INTEGER,
    begin INTEGER NOT NULL,
    end INTEGER,
    tags TEXT,
    priority TEXT,
    scheduled TEXT,
    deadline TEXT,
    FOREIGN KEY(filename_id) REFERENCES files(rowid) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_headlines_filename ON headlines(filename_id);
CREATE INDEX IF NOT EXISTS idx_headlines_todo ON headlines(todo_keyword);
CREATE INDEX IF NOT EXISTS idx_headlines_tags ON headlines(tags);

-- Tags table
CREATE TABLE IF NOT EXISTS tags (
    rowid INTEGER PRIMARY KEY,
    tag TEXT UNIQUE NOT NULL
);

-- Headline tags junction table
CREATE TABLE IF NOT EXISTS headline_tags (
    rowid INTEGER PRIMARY KEY,
    headline_id INTEGER NOT NULL,
    tag_id INTEGER NOT NULL,
    FOREIGN KEY(headline_id) REFERENCES headlines(rowid) ON DELETE CASCADE,
    FOREIGN KEY(tag_id) REFERENCES tags(rowid) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_headline_tags_headline ON headline_tags(headline_id);
CREATE INDEX IF NOT EXISTS idx_headline_tags_tag ON headline_tags(tag_id);

-- Properties table
CREATE TABLE IF NOT EXISTS properties (
    rowid INTEGER PRIMARY KEY,
    property TEXT UNIQUE NOT NULL
);

-- Headline properties table
CREATE TABLE IF NOT EXISTS headline_properties (
    rowid INTEGER PRIMARY KEY,
    headline_id INTEGER NOT NULL,
    property_id INTEGER NOT NULL,
    value TEXT,
    FOREIGN KEY(headline_id) REFERENCES headlines(rowid) ON DELETE CASCADE,
    FOREIGN KEY(property_id) REFERENCES properties(rowid) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_headline_properties_headline ON headline_properties(headline_id);
CREATE INDEX IF NOT EXISTS idx_headline_properties_property ON headline_properties(property_id);

-- Keywords table
CREATE TABLE IF NOT EXISTS keywords (
    rowid INTEGER PRIMARY KEY,
    keyword TEXT UNIQUE NOT NULL
);

-- File keywords table
CREATE TABLE IF NOT EXISTS file_keywords (
    rowid INTEGER PRIMARY KEY,
    filename_id INTEGER NOT NULL,
    keyword_id INTEGER NOT NULL,
    value TEXT,
    begin INTEGER,
    FOREIGN KEY(filename_id) REFERENCES files(rowid) ON DELETE CASCADE,
    FOREIGN KEY(keyword_id) REFERENCES keywords(rowid) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_file_keywords_filename ON file_keywords(filename_id);

-- Links table
CREATE TABLE IF NOT EXISTS links (
    rowid INTEGER PRIMARY KEY,
    filename_id INTEGER NOT NULL,
    type TEXT,
    path TEXT,
    raw_link TEXT,
    description TEXT,
    search_option TEXT,
    begin INTEGER,
    FOREIGN KEY(filename_id) REFERENCES files(rowid) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_links_filename ON links(filename_id);
CREATE INDEX IF NOT EXISTS idx_links_type ON links(type);

-- Hashtags table
CREATE TABLE IF NOT EXISTS hashtags (
    rowid INTEGER PRIMARY KEY,
    hashtag TEXT UNIQUE NOT NULL
);

-- File hashtags table
CREATE TABLE IF NOT EXISTS file_hashtags (
    rowid INTEGER PRIMARY KEY,
    filename_id INTEGER NOT NULL,
    hashtag_id INTEGER NOT NULL,
    begin INTEGER,
    FOREIGN KEY(filename_id) REFERENCES files(rowid) ON DELETE CASCADE,
    FOREIGN KEY(hashtag_id) REFERENCES hashtags(rowid) ON DELETE CASCADE
);

-- @labels table
CREATE TABLE IF NOT EXISTS atlabels (
    rowid INTEGER PRIMARY KEY,
    atlabel TEXT UNIQUE NOT NULL
);

-- File @labels table
CREATE TABLE IF NOT EXISTS file_atlabels (
    rowid INTEGER PRIMARY KEY,
    filename_id INTEGER NOT NULL,
    atlabel_id INTEGER NOT NULL,
    begin INTEGER,
    FOREIGN KEY(filename_id) REFERENCES files(rowid) ON DELETE CASCADE,
    FOREIGN KEY(atlabel_id) REFERENCES atlabels(rowid) ON DELETE CASCADE
);

-- Email addresses table
CREATE TABLE IF NOT EXISTS email_addresses (
    rowid INTEGER PRIMARY KEY,
    email_address TEXT UNIQUE NOT NULL
);

-- File email addresses table
CREATE TABLE IF NOT EXISTS file_email_addresses (
    rowid INTEGER PRIMARY KEY,
    filename_id INTEGER NOT NULL,
    email_address_id INTEGER NOT NULL,
    begin INTEGER,
    FOREIGN KEY(filename_id) REFERENCES files(rowid) ON DELETE CASCADE,
    FOREIGN KEY(email_address_id) REFERENCES email_addresses(rowid) ON DELETE CASCADE
);

-- Src blocks table
CREATE TABLE IF NOT EXISTS src_blocks (
    rowid INTEGER PRIMARY KEY,
    filename_id INTEGER NOT NULL,
    language TEXT,
    contents TEXT,
    begin INTEGER,
    FOREIGN KEY(filename_id) REFERENCES files(rowid) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_src_blocks_language ON src_blocks(language);

-- Timestamps table
CREATE TABLE IF NOT EXISTS timestamps (
    rowid INTEGER PRIMARY KEY,
    filename_id INTEGER NOT NULL,
    ts TEXT,
    type TEXT,
    context TEXT,
    begin INTEGER,
    FOREIGN KEY(filename_id) REFERENCES files(rowid) ON DELETE CASCADE
);

-- Chunks table for semantic search
CREATE TABLE IF NOT EXISTS chunks (
    rowid INTEGER PRIMARY KEY,
    filename_id INTEGER NOT NULL,
    headline_id INTEGER,
    chunk_text TEXT NOT NULL,
    chunk_type TEXT,
    begin_line INTEGER NOT NULL,
    end_line INTEGER NOT NULL,
    char_offset INTEGER,
    FOREIGN KEY(filename_id) REFERENCES files(rowid) ON DELETE CASCADE,
    FOREIGN KEY(headline_id) REFERENCES headlines(rowid) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_chunks_filename ON chunks(filename_id);
CREATE INDEX IF NOT EXISTS idx_chunks_headline ON chunks(headline_id);

-- Embeddings table
CREATE TABLE IF NOT EXISTS embeddings (
    rowid INTEGER PRIMARY KEY,
    chunk_id INTEGER NOT NULL,
    embedding_model TEXT NOT NULL,
    embedding_vector BLOB NOT NULL,
    embedding_dim INTEGER NOT NULL,
    created_at TEXT,
    FOREIGN KEY(chunk_id) REFERENCES chunks(rowid) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_embeddings_chunk ON embeddings(chunk_id);
CREATE INDEX IF NOT EXISTS idx_embeddings_model ON embeddings(embedding_model);

-- Images table
CREATE TABLE IF NOT EXISTS images (
    rowid INTEGER PRIMARY KEY,
    filename_id INTEGER NOT NULL,
    image_path TEXT NOT NULL,
    image_type TEXT,
    width INTEGER,
    height INTEGER,
    file_size INTEGER,
    begin INTEGER,
    ocr_text TEXT,
    FOREIGN KEY(filename_id) REFERENCES files(rowid) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_images_path ON images(image_path);

-- Image embeddings table (CLIP)
CREATE TABLE IF NOT EXISTS image_embeddings (
    rowid INTEGER PRIMARY KEY,
    image_id INTEGER NOT NULL,
    clip_model TEXT NOT NULL,
    embedding_vector BLOB NOT NULL,
    embedding_dim INTEGER NOT NULL,
    created_at TEXT,
    FOREIGN KEY(image_id) REFERENCES images(rowid) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_image_embeddings_image ON image_embeddings(image_id);
CREATE INDEX IF NOT EXISTS idx_image_embeddings_model ON image_embeddings(clip_model);

-- Full-text search virtual table
CREATE VIRTUAL TABLE IF NOT EXISTS fts_content USING fts5(
    filename,
    title,
    content,
    tags,
    content='chunks',
    content_rowid='rowid'
);
"""
```

**Step 4: Create database service**

Create `python/org_db_server/services/database.py`:
```python
"""Database service for org-db."""
import sqlite3
from pathlib import Path
from typing import Optional
from datetime import datetime

from org_db_server.models.db_models import SCHEMA

class Database:
    """Database connection and operations."""

    def __init__(self, db_path: Path):
        """Initialize database connection and create schema if needed."""
        self.db_path = db_path
        self.conn = sqlite3.connect(str(db_path), check_same_thread=False)
        self.conn.row_factory = sqlite3.Row

        # Enable foreign keys
        self.conn.execute("PRAGMA foreign_keys = ON")

        # Initialize schema
        self._initialize_schema()

    def _initialize_schema(self):
        """Create all tables if they don't exist."""
        cursor = self.conn.cursor()
        cursor.executescript(SCHEMA)
        self.conn.commit()

    def close(self):
        """Close database connection."""
        if self.conn:
            self.conn.close()

    def get_or_create_file_id(self, filename: str, md5: str, file_size: int) -> int:
        """Get file ID or create new file entry."""
        cursor = self.conn.cursor()

        # Try to get existing file
        cursor.execute("SELECT rowid FROM files WHERE filename = ?", (filename,))
        row = cursor.fetchone()

        if row:
            # Update existing file
            cursor.execute(
                """UPDATE files SET md5 = ?, last_updated = ?, file_size = ?, indexed_at = ?
                   WHERE rowid = ?""",
                (md5, datetime.now().isoformat(), file_size, datetime.now().isoformat(), row[0])
            )
            self.conn.commit()
            return row[0]
        else:
            # Create new file
            cursor.execute(
                """INSERT INTO files (filename, md5, last_updated, file_size, indexed_at)
                   VALUES (?, ?, ?, ?, ?)""",
                (filename, md5, datetime.now().isoformat(), file_size, datetime.now().isoformat())
            )
            self.conn.commit()
            return cursor.lastrowid
```

**Step 5: Run test to verify it passes**

Run: `cd python && uv run pytest tests/test_database.py::test_database_initialization -v`

Expected: PASS

**Step 6: Add more database tests**

Add to `tests/test_database.py`:
```python
def test_get_or_create_file_id(temp_db):
    """Test getting or creating file IDs."""
    # Create new file
    file_id1 = temp_db.get_or_create_file_id("/test/file.org", "abc123", 1024)
    assert file_id1 > 0

    # Get existing file
    file_id2 = temp_db.get_or_create_file_id("/test/file.org", "abc123", 1024)
    assert file_id1 == file_id2

    # Create different file
    file_id3 = temp_db.get_or_create_file_id("/test/other.org", "def456", 2048)
    assert file_id3 != file_id1
```

**Step 7: Run new test**

Run: `cd python && uv run pytest tests/test_database.py::test_get_or_create_file_id -v`

Expected: PASS

**Step 8: Commit database layer**

```bash
git add python/org_db_server/models/ python/org_db_server/services/ tests/
git commit -m "feat: add libsql database schema and service layer"
```

---

## Phase 3: Emacs Org Parsing & JSON Serialization

### Task 3.1: Org Element to JSON Converter

**Files:**
- Create: `elisp/org-db-v3-parse.el`
- Create: `tests/org-db-v3-parse-test.el`

**Step 1: Write test for parsing headlines**

Create `tests/org-db-v3-parse-test.el`:
```elisp
;;; org-db-v3-parse-test.el --- Tests for org parsing -*- lexical-binding: t; -*-

(require 'ert)
(require 'org-db-v3-parse)

(ert-deftest org-db-v3-test-parse-headlines ()
  "Test parsing headlines to JSON structure."
  (with-temp-buffer
    (insert "* TODO Test Heading :tag1:tag2:\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: test-id\n")
    (insert ":END:\n")
    (insert "Content here.\n")
    (org-mode)

    (let* ((parse-tree (org-element-parse-buffer))
           (headlines (org-db-v3-parse-headlines parse-tree)))

      (should (= (length headlines) 1))
      (let ((hl (car headlines)))
        (should (equal (plist-get hl :title) "Test Heading"))
        (should (equal (plist-get hl :level) 1))
        (should (equal (plist-get hl :todo-keyword) "TODO"))
        (should (equal (plist-get hl :tags) ":tag1:tag2:"))))))

;;; org-db-v3-parse-test.el ends here
```

**Step 2: Run test to verify it fails**

Run in Emacs: `M-x eval-buffer` in test file, then `M-x ert RET org-db-v3-test-parse-headlines RET`

Expected: FAIL with "Cannot open load file: No such file or directory, org-db-v3-parse"

**Step 3: Create parsing module**

Create `elisp/org-db-v3-parse.el`:
```elisp
;;; org-db-v3-parse.el --- Org parsing to JSON -*- lexical-binding: t; -*-

;;; Commentary:
;; Functions to parse org-mode files and convert to JSON for the server.

;;; Code:

(require 'org)
(require 'org-element)
(require 'json)

(defun org-db-v3-parse-headlines (parse-tree)
  "Extract headlines from PARSE-TREE as a list of plists."
  (org-element-map parse-tree 'headline
    (lambda (hl)
      (let* ((begin (org-element-property :begin hl))
             (end (org-element-property :end hl))
             (tags (org-element-property :tags hl))
             (scheduled (org-element-property :scheduled hl))
             (deadline (org-element-property :deadline hl))
             (properties (save-excursion
                          (goto-char begin)
                          (org-entry-properties))))
        (list :title (org-element-property :raw-value hl)
              :level (org-element-property :level hl)
              :todo-keyword (org-element-property :todo-keyword hl)
              :todo-type (symbol-name (or (org-element-property :todo-type hl) 'nil))
              :archivedp (org-element-property :archivedp hl)
              :commentedp (org-element-property :commentedp hl)
              :begin begin
              :end end
              :tags (when tags
                     (concat ":" (mapconcat #'identity tags ":") ":"))
              :priority (when-let ((p (org-element-property :priority hl)))
                         (char-to-string p))
              :scheduled (when scheduled
                          (org-timestamp-format scheduled "%Y-%m-%d %H:%M:%S"))
              :deadline (when deadline
                         (org-timestamp-format deadline "%Y-%m-%d %H:%M:%S"))
              :properties properties)))))

(defun org-db-v3-parse-links (parse-tree)
  "Extract links from PARSE-TREE as a list of plists."
  (org-element-map parse-tree 'link
    (lambda (link)
      (list :type (org-element-property :type link)
            :path (org-element-property :path link)
            :raw-link (org-element-property :raw-link link)
            :description (when (org-element-property :contents-begin link)
                          (buffer-substring-no-properties
                           (org-element-property :contents-begin link)
                           (org-element-property :contents-end link)))
            :search-option (org-element-property :search-option link)
            :begin (org-element-property :begin link)))))

(defun org-db-v3-parse-keywords (parse-tree)
  "Extract keywords from PARSE-TREE as a list of plists."
  (org-element-map parse-tree 'keyword
    (lambda (kw)
      (list :key (upcase (org-element-property :key kw))
            :value (org-element-property :value kw)
            :begin (org-element-property :begin kw)))))

(defun org-db-v3-parse-src-blocks (parse-tree)
  "Extract src blocks from PARSE-TREE as a list of plists."
  (org-element-map parse-tree 'src-block
    (lambda (src)
      (list :language (org-element-property :language src)
            :contents (org-element-property :value src)
            :begin (org-element-property :begin src)))))

(defun org-db-v3-parse-buffer-to-json ()
  "Parse current org buffer and return JSON string for server."
  (let* ((parse-tree (org-element-parse-buffer))
         (data (list :filename (buffer-file-name)
                     :md5 (md5 (current-buffer))
                     :content (buffer-string)
                     :headlines (org-db-v3-parse-headlines parse-tree)
                     :links (org-db-v3-parse-links parse-tree)
                     :keywords (org-db-v3-parse-keywords parse-tree)
                     :src-blocks (org-db-v3-parse-src-blocks parse-tree))))
    (json-encode data)))

(provide 'org-db-v3-parse)
;;; org-db-v3-parse.el ends here
```

**Step 4: Run test to verify it passes**

Run in Emacs: `M-x load-file RET elisp/org-db-v3-parse.el RET`
Then: `M-x eval-buffer` in test file, then `M-x ert RET org-db-v3-test-parse-headlines RET`

Expected: PASS

**Step 5: Add test for full JSON conversion**

Add to `tests/org-db-v3-parse-test.el`:
```elisp
(ert-deftest org-db-v3-test-parse-to-json ()
  "Test parsing buffer to JSON string."
  (with-temp-buffer
    (insert "#+TITLE: Test\n")
    (insert "* Heading\n")
    (insert "[[https://example.com][link]]\n")
    (org-mode)

    (let* ((json-str (org-db-v3-parse-buffer-to-json))
           (data (json-read-from-string json-str)))

      (should (assoc 'headlines data))
      (should (assoc 'links data))
      (should (assoc 'keywords data)))))
```

**Step 6: Run new test**

Expected: PASS

**Step 7: Commit parsing functionality**

```bash
git add elisp/org-db-v3-parse.el tests/org-db-v3-parse-test.el
git commit -m "feat: add org-element to JSON parsing"
```

---

## Phase 4: API Endpoints for Indexing

### Task 4.1: File Indexing Endpoint

**Files:**
- Create: `python/org_db_server/models/schemas.py`
- Create: `python/org_db_server/api/indexing.py`
- Create: `tests/test_indexing_api.py`
- Modify: `python/org_db_server/main.py`

**Step 1: Write test for indexing endpoint**

Create `tests/test_indexing_api.py`:
```python
"""Tests for indexing API endpoints."""
import pytest
from fastapi.testclient import TestClient
from pathlib import Path

from org_db_server.main import app
from org_db_server.services.database import Database
from org_db_server.config import settings

@pytest.fixture
def client(tmp_path):
    """Create test client with temporary database."""
    # Override database path for testing
    test_db_path = tmp_path / "test.db"
    settings.db_path = test_db_path

    client = TestClient(app)
    yield client

def test_index_file_endpoint(client):
    """Test POST /api/index/file endpoint."""
    payload = {
        "filename": "/test/sample.org",
        "md5": "abc123",
        "file_size": 1024,
        "headlines": [
            {
                "title": "Test Heading",
                "level": 1,
                "todo_keyword": "TODO",
                "tags": ":test:",
                "begin": 10,
                "end": 50
            }
        ],
        "links": [],
        "keywords": [
            {"key": "TITLE", "value": "Test", "begin": 0}
        ],
        "src_blocks": []
    }

    response = client.post("/api/index/file", json=payload)

    assert response.status_code == 200
    data = response.json()
    assert "file_id" in data
    assert data["status"] == "indexed"
```

**Step 2: Run test to verify it fails**

Run: `cd python && uv run pytest tests/test_indexing_api.py::test_index_file_endpoint -v`

Expected: FAIL with 404 Not Found (endpoint doesn't exist)

**Step 3: Create Pydantic schemas**

Create `python/org_db_server/models/schemas.py`:
```python
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
    language: str
    contents: str
    begin: int

class IndexFileRequest(BaseModel):
    """Request to index a file."""
    filename: str
    md5: str
    file_size: int
    headlines: List[HeadlineData] = Field(default_factory=list)
    links: List[LinkData] = Field(default_factory=list)
    keywords: List[KeywordData] = Field(default_factory=list)
    src_blocks: List[SrcBlockData] = Field(default_factory=list)

class IndexFileResponse(BaseModel):
    """Response from indexing a file."""
    file_id: int
    status: str
    headlines_count: int
    links_count: int
```

**Step 4: Create indexing API endpoint**

Create `python/org_db_server/api/indexing.py`:
```python
"""Indexing API endpoints."""
from fastapi import APIRouter, HTTPException
from datetime import datetime

from org_db_server.models.schemas import IndexFileRequest, IndexFileResponse
from org_db_server.services.database import Database
from org_db_server.config import settings

router = APIRouter(prefix="/api/index", tags=["indexing"])

# Global database instance (will be improved later with dependency injection)
db = Database(settings.db_path)

@router.post("/file", response_model=IndexFileResponse)
async def index_file(request: IndexFileRequest):
    """Index an org file."""
    try:
        # Get or create file entry
        file_id = db.get_or_create_file_id(
            request.filename,
            request.md5,
            request.file_size
        )

        cursor = db.conn.cursor()

        # Delete existing data for this file (we'll re-index everything)
        cursor.execute("DELETE FROM headlines WHERE filename_id = ?", (file_id,))
        cursor.execute("DELETE FROM links WHERE filename_id = ?", (file_id,))
        cursor.execute("DELETE FROM file_keywords WHERE filename_id = ?", (file_id,))
        cursor.execute("DELETE FROM src_blocks WHERE filename_id = ?", (file_id,))

        # Insert headlines
        for hl in request.headlines:
            cursor.execute(
                """INSERT INTO headlines (filename_id, title, level, todo_keyword, todo_type,
                   archivedp, commentedp, begin, end, tags, priority, scheduled, deadline)
                   VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)""",
                (file_id, hl.title, hl.level, hl.todo_keyword, hl.todo_type,
                 hl.archivedp, hl.commentedp, hl.begin, hl.end, hl.tags,
                 hl.priority, hl.scheduled, hl.deadline)
            )
            headline_id = cursor.lastrowid

            # Insert properties if any
            if hl.properties:
                for prop_key, prop_value in hl.properties.items():
                    # Get or create property
                    cursor.execute("SELECT rowid FROM properties WHERE property = ?", (prop_key,))
                    prop_row = cursor.fetchone()

                    if prop_row:
                        property_id = prop_row[0]
                    else:
                        cursor.execute("INSERT INTO properties (property) VALUES (?)", (prop_key,))
                        property_id = cursor.lastrowid

                    # Insert headline property
                    cursor.execute(
                        "INSERT INTO headline_properties (headline_id, property_id, value) VALUES (?, ?, ?)",
                        (headline_id, property_id, prop_value)
                    )

        # Insert links
        for link in request.links:
            cursor.execute(
                """INSERT INTO links (filename_id, type, path, raw_link, description, search_option, begin)
                   VALUES (?, ?, ?, ?, ?, ?, ?)""",
                (file_id, link.type, link.path, link.raw_link, link.description, link.search_option, link.begin)
            )

        # Insert keywords
        for kw in request.keywords:
            # Get or create keyword
            cursor.execute("SELECT rowid FROM keywords WHERE keyword = ?", (kw.key,))
            kw_row = cursor.fetchone()

            if kw_row:
                keyword_id = kw_row[0]
            else:
                cursor.execute("INSERT INTO keywords (keyword) VALUES (?)", (kw.key,))
                keyword_id = cursor.lastrowid

            # Insert file keyword
            cursor.execute(
                "INSERT INTO file_keywords (filename_id, keyword_id, value, begin) VALUES (?, ?, ?, ?)",
                (file_id, keyword_id, kw.value, kw.begin)
            )

        # Insert src blocks
        for src in request.src_blocks:
            cursor.execute(
                "INSERT INTO src_blocks (filename_id, language, contents, begin) VALUES (?, ?, ?, ?)",
                (file_id, src.language, src.contents, src.begin)
            )

        db.conn.commit()

        return IndexFileResponse(
            file_id=file_id,
            status="indexed",
            headlines_count=len(request.headlines),
            links_count=len(request.links)
        )

    except Exception as e:
        db.conn.rollback()
        raise HTTPException(status_code=500, detail=str(e))
```

**Step 5: Register router in main app**

Modify `python/org_db_server/main.py`:
```python
"""FastAPI server for org-db v3."""
from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware

from org_db_server.api import indexing

app = FastAPI(title="org-db Server", version="0.1.0")

# Allow Emacs to connect from localhost
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Include routers
app.include_router(indexing.router)

@app.get("/health")
async def health_check():
    """Health check endpoint."""
    return {"status": "ok", "version": "0.1.0"}

@app.get("/")
async def root():
    """Root endpoint."""
    return {"message": "org-db v3 server running"}
```

**Step 6: Run test to verify it passes**

Run: `cd python && uv run pytest tests/test_indexing_api.py::test_index_file_endpoint -v`

Expected: PASS

**Step 7: Commit indexing API**

```bash
git add python/org_db_server/models/schemas.py python/org_db_server/api/ python/org_db_server/main.py tests/test_indexing_api.py
git commit -m "feat: add file indexing API endpoint"
```

---

## Phase 5: Emacs Async Indexing

### Task 5.1: Async HTTP Client & Queue Processing

**Files:**
- Create: `elisp/org-db-v3-client.el`
- Create: `elisp/org-db-v3-server.el`
- Modify: `elisp/org-db-v3.el`

**Step 1: Create server management module**

Create `elisp/org-db-v3-server.el`:
```elisp
;;; org-db-v3-server.el --- Server management -*- lexical-binding: t; -*-

;;; Commentary:
;; Functions to start, stop, and check the org-db server.

;;; Code:

(require 'org-db-v3)

(defcustom org-db-v3-python-command "uv"
  "Command to run Python (uv, python3, etc)."
  :type 'string
  :group 'org-db-v3)

(defcustom org-db-v3-server-directory
  (expand-file-name "python" (file-name-directory (or load-file-name buffer-file-name)))
  "Directory containing the Python server code."
  :type 'directory
  :group 'org-db-v3)

(defun org-db-v3-start-server ()
  "Start the org-db server."
  (interactive)
  (if (org-db-v3-server-running-p)
      (message "org-db server already running")
    (let* ((default-directory org-db-v3-server-directory)
           (process-name "org-db-server")
           (buffer-name "*org-db-server*")
           (cmd (list org-db-v3-python-command "run" "uvicorn"
                     "org_db_server.main:app" "--reload"
                     "--host" org-db-v3-server-host
                     "--port" (number-to-string org-db-v3-server-port))))

      (setq org-db-v3-server-process
            (make-process
             :name process-name
             :buffer buffer-name
             :command cmd
             :sentinel #'org-db-v3-server-sentinel))

      ;; Wait a bit for server to start
      (sleep-for 2)

      (if (org-db-v3-server-running-p)
          (message "org-db server started on %s:%d"
                   org-db-v3-server-host org-db-v3-server-port)
        (error "Failed to start org-db server. Check *org-db-server* buffer")))))

(defun org-db-v3-stop-server ()
  "Stop the org-db server."
  (interactive)
  (when (and org-db-v3-server-process
             (process-live-p org-db-v3-server-process))
    (kill-process org-db-v3-server-process)
    (setq org-db-v3-server-process nil)
    (message "org-db server stopped")))

(defun org-db-v3-server-sentinel (process event)
  "Sentinel for server PROCESS EVENT."
  (when (string-match-p "\\(finished\\|exited\\)" event)
    (message "org-db server process %s" event)
    (setq org-db-v3-server-process nil)))

(defun org-db-v3-ensure-server ()
  "Ensure server is running, start if needed."
  (unless (org-db-v3-server-running-p)
    (when org-db-v3-auto-start-server
      (org-db-v3-start-server))))

(provide 'org-db-v3-server)
;;; org-db-v3-server.el ends here
```

**Step 2: Create HTTP client module**

Create `elisp/org-db-v3-client.el`:
```elisp
;;; org-db-v3-client.el --- HTTP client for org-db -*- lexical-binding: t; -*-

;;; Commentary:
;; Async HTTP client using plz.el to communicate with the server.

;;; Code:

(require 'plz)
(require 'json)
(require 'org-db-v3)
(require 'org-db-v3-parse)
(require 'org-db-v3-server)

(defvar org-db-v3-index-queue nil
  "Queue of files pending indexing.")

(defvar org-db-v3-indexing-in-progress nil
  "Whether indexing is currently in progress.")

(defun org-db-v3-add-to-queue (filename)
  "Add FILENAME to indexing queue."
  (unless (member filename org-db-v3-index-queue)
    (push filename org-db-v3-index-queue)))

(defun org-db-v3-index-file-async (filename)
  "Index FILENAME asynchronously."
  (org-db-v3-ensure-server)

  (with-current-buffer (find-file-noselect filename)
    (let ((json-data (org-db-v3-parse-buffer-to-json)))
      (plz 'post (concat (org-db-v3-server-url) "/api/index/file")
        :headers '(("Content-Type" . "application/json"))
        :body json-data
        :as #'json-read
        :then (lambda (response)
                (message "Indexed %s (%d headlines)"
                         filename
                         (alist-get 'headlines_count response)))
        :else (lambda (error)
                (message "Error indexing %s: %s" filename error))))))

(defun org-db-v3-process-queue ()
  "Process the indexing queue."
  (when (and org-db-v3-index-queue
             (not org-db-v3-indexing-in-progress)
             (current-idle-time))
    (setq org-db-v3-indexing-in-progress t)
    (let ((filename (pop org-db-v3-index-queue)))
      (when (file-exists-p filename)
        (org-db-v3-index-file-async filename))
      (setq org-db-v3-indexing-in-progress nil))))

(defvar org-db-v3-idle-timer nil
  "Timer for processing the queue.")

(defun org-db-v3-start-queue-processing ()
  "Start the idle timer for queue processing."
  (unless org-db-v3-idle-timer
    (setq org-db-v3-idle-timer
          (run-with-idle-timer 5 t #'org-db-v3-process-queue))))

(defun org-db-v3-stop-queue-processing ()
  "Stop the idle timer."
  (when org-db-v3-idle-timer
    (cancel-timer org-db-v3-idle-timer)
    (setq org-db-v3-idle-timer nil)))

(provide 'org-db-v3-client)
;;; org-db-v3-client.el ends here
```

**Step 3: Add hooks to main package**

Modify `elisp/org-db-v3.el` to add:
```elisp
(require 'org-db-v3-parse)
(require 'org-db-v3-client)
(require 'org-db-v3-server)

(defun org-db-v3-hook-function ()
  "Hook function for org-mode files."
  (when (and (buffer-file-name)
             (or (string-suffix-p ".org" (buffer-file-name))
                 (string-suffix-p ".org_archive" (buffer-file-name))))
    (org-db-v3-add-to-queue (buffer-file-name))
    (add-hook 'after-save-hook #'org-db-v3-after-save-hook nil t)))

(defun org-db-v3-after-save-hook ()
  "Hook to run after saving an org file."
  (when (buffer-file-name)
    (org-db-v3-add-to-queue (buffer-file-name))))

(defun org-db-v3-enable ()
  "Enable org-db v3."
  (interactive)
  (add-hook 'org-mode-hook #'org-db-v3-hook-function)
  (org-db-v3-start-queue-processing)
  (message "org-db v3 enabled"))

(defun org-db-v3-disable ()
  "Disable org-db v3."
  (interactive)
  (remove-hook 'org-mode-hook #'org-db-v3-hook-function)
  (org-db-v3-stop-queue-processing)
  (message "org-db v3 disabled"))
```

**Step 4: Test manually**

1. Start Emacs
2. Load files: `M-x load-file` for each elisp file
3. Run: `M-x org-db-v3-enable`
4. Open tests/fixtures/sample.org
5. Save the file
6. Check *Messages* buffer for "Indexed..." message

Expected: File gets indexed successfully

**Step 5: Commit async indexing**

```bash
git add elisp/
git commit -m "feat: add async indexing with queue processing"
```

---

## Phase 6: Text Chunking & Embeddings

### Task 6.1: Text Chunking Service

**Files:**
- Create: `python/org_db_server/services/chunking.py`
- Create: `tests/test_chunking.py`

**Step 1: Write test for chunking**

Create `tests/test_chunking.py`:
```python
"""Tests for text chunking."""
import pytest
from org_db_server.services.chunking import chunk_text

def test_chunk_by_paragraphs():
    """Test chunking text into paragraphs."""
    text = """This is paragraph one.

This is paragraph two with more content.

This is paragraph three."""

    chunks = chunk_text(text, method="paragraph")

    assert len(chunks) == 3
    assert chunks[0]["text"].startswith("This is paragraph one")
    assert chunks[0]["chunk_type"] == "paragraph"

def test_chunk_respects_size_limit():
    """Test that chunks respect maximum size."""
    text = "word " * 1000  # Very long text

    chunks = chunk_text(text, method="fixed", chunk_size=100, chunk_overlap=10)

    for chunk in chunks:
        assert len(chunk["text"]) <= 120  # Size + some overlap
```

**Step 2: Run test to verify it fails**

Run: `cd python && uv run pytest tests/test_chunking.py -v`

Expected: FAIL with "No module named 'org_db_server.services.chunking'"

**Step 3: Implement chunking service**

Create `python/org_db_server/services/chunking.py`:
```python
"""Text chunking service."""
import re
from typing import List, Dict, Literal

def chunk_text(
    text: str,
    method: Literal["paragraph", "fixed"] = "paragraph",
    chunk_size: int = 512,
    chunk_overlap: int = 50
) -> List[Dict[str, any]]:
    """
    Chunk text into smaller pieces.

    Args:
        text: Text to chunk
        method: Chunking method ("paragraph" or "fixed")
        chunk_size: Maximum chunk size in characters (for fixed method)
        chunk_overlap: Overlap between chunks in characters

    Returns:
        List of chunk dictionaries with text, chunk_type, begin_line, end_line
    """
    chunks = []

    if method == "paragraph":
        # Split by blank lines (paragraphs)
        paragraphs = re.split(r'\n\s*\n', text)
        current_line = 0

        for para in paragraphs:
            if para.strip():
                line_count = para.count('\n') + 1
                chunks.append({
                    "text": para.strip(),
                    "chunk_type": "paragraph",
                    "begin_line": current_line,
                    "end_line": current_line + line_count
                })
                current_line += line_count + 1  # +1 for blank line

    elif method == "fixed":
        # Fixed-size chunks with overlap
        pos = 0
        line = 0

        while pos < len(text):
            end_pos = min(pos + chunk_size, len(text))
            chunk_text = text[pos:end_pos]

            # Count lines in chunk
            line_count = chunk_text.count('\n')

            chunks.append({
                "text": chunk_text,
                "chunk_type": "fixed",
                "begin_line": line,
                "end_line": line + line_count
            })

            line += line_count
            pos = end_pos - chunk_overlap if end_pos < len(text) else end_pos

    return chunks
```

**Step 4: Run tests to verify they pass**

Run: `cd python && uv run pytest tests/test_chunking.py -v`

Expected: PASS

**Step 5: Commit chunking service**

```bash
git add python/org_db_server/services/chunking.py tests/test_chunking.py
git commit -m "feat: add text chunking service"
```

### Task 6.2: Embedding Service

**Files:**
- Create: `python/org_db_server/services/embeddings.py`
- Create: `tests/test_embeddings.py`

**Step 1: Write test for embeddings**

Create `tests/test_embeddings.py`:
```python
"""Tests for embedding service."""
import pytest
import numpy as np
from org_db_server.services.embeddings import EmbeddingService

@pytest.fixture
def embedding_service():
    """Create embedding service with small model for testing."""
    return EmbeddingService(model_name="all-MiniLM-L6-v2")

def test_generate_embedding(embedding_service):
    """Test generating a single embedding."""
    text = "This is a test sentence."

    embedding = embedding_service.generate_embedding(text)

    assert isinstance(embedding, np.ndarray)
    assert embedding.shape[0] == 384  # all-MiniLM-L6-v2 dimension
    assert np.isfinite(embedding).all()

def test_generate_embeddings_batch(embedding_service):
    """Test generating embeddings in batch."""
    texts = ["First sentence.", "Second sentence.", "Third sentence."]

    embeddings = embedding_service.generate_embeddings(texts)

    assert len(embeddings) == 3
    assert all(emb.shape[0] == 384 for emb in embeddings)
```

**Step 2: Run test to verify it fails**

Run: `cd python && uv run pytest tests/test_embeddings.py::test_generate_embedding -v`

Expected: FAIL with "No module named 'org_db_server.services.embeddings'"

**Step 3: Implement embedding service**

Create `python/org_db_server/services/embeddings.py`:
```python
"""Embedding generation service."""
import numpy as np
from typing import List, Union
from sentence_transformers import SentenceTransformer

class EmbeddingService:
    """Service for generating text embeddings."""

    def __init__(self, model_name: str = "all-MiniLM-L6-v2"):
        """Initialize with a specific model."""
        self.model_name = model_name
        self.model = SentenceTransformer(model_name)
        self.dimension = self.model.get_sentence_embedding_dimension()

    def generate_embedding(self, text: str) -> np.ndarray:
        """Generate embedding for a single text."""
        return self.model.encode(text, convert_to_numpy=True)

    def generate_embeddings(self, texts: List[str], batch_size: int = 32) -> List[np.ndarray]:
        """Generate embeddings for multiple texts in batches."""
        embeddings = self.model.encode(
            texts,
            batch_size=batch_size,
            convert_to_numpy=True,
            show_progress_bar=len(texts) > 100
        )
        return [embeddings[i] for i in range(len(texts))]

    def similarity(self, emb1: np.ndarray, emb2: np.ndarray) -> float:
        """Calculate cosine similarity between two embeddings."""
        return float(np.dot(emb1, emb2) / (np.linalg.norm(emb1) * np.linalg.norm(emb2)))

# Global embedding service instance (lazy loaded)
_embedding_service = None

def get_embedding_service(model_name: str = "all-MiniLM-L6-v2") -> EmbeddingService:
    """Get or create the global embedding service."""
    global _embedding_service
    if _embedding_service is None or _embedding_service.model_name != model_name:
        _embedding_service = EmbeddingService(model_name)
    return _embedding_service
```

**Step 4: Run tests to verify they pass**

Run: `cd python && uv run pytest tests/test_embeddings.py -v`

Expected: PASS (may take a minute to download model on first run)

**Step 5: Commit embedding service**

```bash
git add python/org_db_server/services/embeddings.py tests/test_embeddings.py
git commit -m "feat: add embedding generation service"
```

---

## Phase 7: Semantic Search Implementation

### Task 7.1: Integrate Chunking & Embeddings into Indexing

**Files:**
- Modify: `python/org_db_server/api/indexing.py`
- Modify: `python/org_db_server/services/database.py`

**Step 1: Add chunk storage to database service**

Add to `python/org_db_server/services/database.py`:
```python
def store_chunks(self, filename_id: int, chunks: List[Dict], embeddings: List[np.ndarray], model_name: str):
    """Store text chunks and their embeddings."""
    cursor = self.conn.cursor()

    # Delete existing chunks for this file
    cursor.execute("DELETE FROM chunks WHERE filename_id = ?", (filename_id,))

    for chunk_data, embedding in zip(chunks, embeddings):
        # Insert chunk
        cursor.execute(
            """INSERT INTO chunks (filename_id, headline_id, chunk_text, chunk_type, begin_line, end_line, char_offset)
               VALUES (?, ?, ?, ?, ?, ?, ?)""",
            (filename_id, None, chunk_data["text"], chunk_data["chunk_type"],
             chunk_data["begin_line"], chunk_data["end_line"], 0)
        )
        chunk_id = cursor.lastrowid

        # Convert embedding to bytes
        embedding_bytes = embedding.astype(np.float32).tobytes()

        # Insert embedding
        cursor.execute(
            """INSERT INTO embeddings (chunk_id, embedding_model, embedding_vector, embedding_dim, created_at)
               VALUES (?, ?, ?, ?, ?)""",
            (chunk_id, model_name, embedding_bytes, len(embedding), datetime.now().isoformat())
        )

    self.conn.commit()
```

Add import at top:
```python
import numpy as np
```

**Step 2: Modify indexing endpoint to generate embeddings**

Modify `python/org_db_server/api/indexing.py` to add after inserting headlines/links:
```python
from org_db_server.services.chunking import chunk_text
from org_db_server.services.embeddings import get_embedding_service

# ... existing code ...

        # After inserting src blocks, add:

        # Generate chunks from file content
        # For now, we'll chunk based on headlines
        headline_texts = [hl.title for hl in request.headlines]

        if headline_texts:
            # Chunk the text
            all_chunks = []
            for text in headline_texts:
                chunks = chunk_text(text, method="paragraph")
                all_chunks.extend(chunks)

            # Generate embeddings
            embedding_service = get_embedding_service()
            chunk_texts = [c["text"] for c in all_chunks]
            embeddings = embedding_service.generate_embeddings(chunk_texts)

            # Store chunks and embeddings
            db.store_chunks(file_id, all_chunks, embeddings, embedding_service.model_name)

        db.conn.commit()

        return IndexFileResponse(
            file_id=file_id,
            status="indexed",
            headlines_count=len(request.headlines),
            links_count=len(request.links)
        )
```

**Step 3: Test indexing with embeddings**

Modify `tests/test_indexing_api.py` to add:
```python
def test_index_file_with_embeddings(client):
    """Test that indexing generates embeddings."""
    payload = {
        "filename": "/test/sample.org",
        "md5": "abc123",
        "file_size": 1024,
        "headlines": [
            {
                "title": "Machine learning is a subset of artificial intelligence.",
                "level": 1,
                "begin": 10,
                "end": 50
            }
        ],
        "links": [],
        "keywords": [],
        "src_blocks": []
    }

    response = client.post("/api/index/file", json=payload)
    assert response.status_code == 200

    # Verify embeddings were created
    # (Would need database access to fully verify)
```

**Step 4: Run test**

Run: `cd python && uv run pytest tests/test_indexing_api.py -v`

Expected: PASS

**Step 5: Commit embedding integration**

```bash
git add python/org_db_server/
git commit -m "feat: integrate embeddings into indexing pipeline"
```

### Task 7.2: Semantic Search Endpoint

**Files:**
- Create: `python/org_db_server/api/search.py`
- Create: `tests/test_search_api.py`
- Modify: `python/org_db_server/main.py`

**Step 1: Write test for semantic search**

Create `tests/test_search_api.py`:
```python
"""Tests for search API."""
import pytest
from fastapi.testclient import TestClient

from org_db_server.main import app
from org_db_server.config import settings

@pytest.fixture
def client_with_data(tmp_path):
    """Create client with indexed data."""
    test_db_path = tmp_path / "test.db"
    settings.db_path = test_db_path

    client = TestClient(app)

    # Index a test file
    payload = {
        "filename": "/test/ml.org",
        "md5": "abc123",
        "file_size": 1024,
        "headlines": [
            {
                "title": "Deep learning uses neural networks for pattern recognition.",
                "level": 1,
                "begin": 10,
                "end": 50
            },
            {
                "title": "Cooking pasta requires boiling water.",
                "level": 1,
                "begin": 60,
                "end": 90
            }
        ],
        "links": [],
        "keywords": [],
        "src_blocks": []
    }
    client.post("/api/index/file", json=payload)

    yield client

def test_semantic_search(client_with_data):
    """Test semantic search endpoint."""
    response = client_with_data.post(
        "/api/search/semantic",
        json={"query": "machine learning algorithms", "limit": 10}
    )

    assert response.status_code == 200
    data = response.json()
    assert "results" in data
    assert len(data["results"]) > 0

    # First result should be about deep learning, not cooking
    first_result = data["results"][0]
    assert "neural" in first_result["chunk_text"].lower() or "deep" in first_result["chunk_text"].lower()
    assert first_result["similarity"] > 0.5
```

**Step 2: Run test to verify it fails**

Run: `cd python && uv run pytest tests/test_search_api.py::test_semantic_search -v`

Expected: FAIL with 404 (endpoint doesn't exist)

**Step 3: Implement semantic search endpoint**

Create `python/org_db_server/api/search.py`:
```python
"""Search API endpoints."""
from typing import List, Optional
from fastapi import APIRouter, HTTPException
import numpy as np

from org_db_server.models.schemas import SemanticSearchRequest, SemanticSearchResponse, SearchResult
from org_db_server.services.database import Database
from org_db_server.services.embeddings import get_embedding_service
from org_db_server.config import settings

router = APIRouter(prefix="/api/search", tags=["search"])

db = Database(settings.db_path)

@router.post("/semantic", response_model=SemanticSearchResponse)
async def semantic_search(request: SemanticSearchRequest):
    """Perform semantic search using embeddings."""
    try:
        # Generate embedding for query
        embedding_service = get_embedding_service(request.embedding_model)
        query_embedding = embedding_service.generate_embedding(request.query)

        # Fetch all embeddings from database
        cursor = db.conn.cursor()
        cursor.execute(
            """SELECT e.chunk_id, e.embedding_vector, c.chunk_text, c.begin_line,
                      f.filename, h.title
               FROM embeddings e
               JOIN chunks c ON e.chunk_id = c.rowid
               JOIN files f ON c.filename_id = f.rowid
               LEFT JOIN headlines h ON c.headline_id = h.rowid
               WHERE e.embedding_model = ?""",
            (request.embedding_model,)
        )

        results = []
        for row in cursor.fetchall():
            chunk_id, embedding_bytes, chunk_text, begin_line, filename, headline = row

            # Convert bytes back to numpy array
            stored_embedding = np.frombuffer(embedding_bytes, dtype=np.float32)

            # Calculate similarity
            similarity = embedding_service.similarity(query_embedding, stored_embedding)

            results.append({
                "chunk_id": chunk_id,
                "chunk_text": chunk_text,
                "filename": filename,
                "headline": headline,
                "similarity": similarity,
                "begin_line": begin_line
            })

        # Sort by similarity (highest first)
        results.sort(key=lambda x: x["similarity"], reverse=True)

        # Limit results
        results = results[:request.limit]

        return SemanticSearchResponse(results=results)

    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))
```

**Step 4: Add search schemas**

Add to `python/org_db_server/models/schemas.py`:
```python
class SemanticSearchRequest(BaseModel):
    """Request for semantic search."""
    query: str
    limit: int = 20
    embedding_model: str = "all-MiniLM-L6-v2"

class SearchResult(BaseModel):
    """A single search result."""
    chunk_id: int
    chunk_text: str
    filename: str
    headline: Optional[str] = None
    similarity: float
    begin_line: int

class SemanticSearchResponse(BaseModel):
    """Response from semantic search."""
    results: List[SearchResult]
```

**Step 5: Register router in main app**

Modify `python/org_db_server/main.py`:
```python
from org_db_server.api import indexing, search

# ... existing code ...

app.include_router(indexing.router)
app.include_router(search.router)
```

**Step 6: Run test to verify it passes**

Run: `cd python && uv run pytest tests/test_search_api.py::test_semantic_search -v`

Expected: PASS

**Step 7: Commit semantic search**

```bash
git add python/org_db_server/api/search.py python/org_db_server/models/schemas.py tests/test_search_api.py python/org_db_server/main.py
git commit -m "feat: add semantic search endpoint"
```

---

## Phase 8: Emacs Search Interface

### Task 8.1: Semantic Search Command

**Files:**
- Create: `elisp/org-db-v3-search.el`
- Create: `elisp/org-db-v3-ui.el`
- Modify: `elisp/org-db-v3.el`

**Step 1: Create search module**

Create `elisp/org-db-v3-search.el`:
```elisp
;;; org-db-v3-search.el --- Search commands -*- lexical-binding: t; -*-

;;; Commentary:
;; Interactive search commands for org-db v3.

;;; Code:

(require 'plz)
(require 'json)
(require 'org-db-v3)
(require 'org-db-v3-server)

(defun org-db-v3-semantic-search (query)
  "Search org files semantically using QUERY."
  (interactive "sSearch: ")
  (org-db-v3-ensure-server)

  (plz 'post (concat (org-db-v3-server-url) "/api/search/semantic")
    :headers '(("Content-Type" . "application/json"))
    :body (json-encode `((query . ,query) (limit . 20)))
    :as #'json-read
    :then (lambda (response)
            (org-db-v3-display-search-results
             (alist-get 'results response)
             query))
    :else (lambda (error)
            (message "Search error: %s" error))))

(defun org-db-v3-display-search-results (results query)
  "Display search RESULTS for QUERY."
  (if (zerop (length results))
      (message "No results found for: %s" query)
    (let* ((candidates
            (mapcar (lambda (result)
                      (let ((similarity (alist-get 'similarity result))
                            (chunk-text (alist-get 'chunk_text result))
                            (filename (alist-get 'filename result))
                            (headline (alist-get 'headline result))
                            (begin-line (alist-get 'begin_line result)))
                        (cons
                         (format "[%.2f] %s :: %s  %s"
                                 similarity
                                 (or headline "")
                                 (truncate-string-to-width chunk-text 60 nil nil "...")
                                 (file-name-nondirectory filename))
                         (list :filename filename
                               :begin-line begin-line
                               :chunk-text chunk-text
                               :similarity similarity))))
                    results)))

      (let ((selection (completing-read
                       (format "Search results (%d): " (length candidates))
                       candidates
                       nil t)))
        (when selection
          (let* ((candidate (cdr (assoc selection candidates)))
                 (filename (plist-get candidate :filename))
                 (begin-line (plist-get candidate :begin-line)))
            (find-file filename)
            (goto-char (point-min))
            (forward-line begin-line)
            (org-show-context)))))))

(provide 'org-db-v3-search)
;;; org-db-v3-search.el ends here
```

**Step 2: Create transient UI**

Create `elisp/org-db-v3-ui.el`:
```elisp
;;; org-db-v3-ui.el --- Transient menu interface -*- lexical-binding: t; -*-

;;; Commentary:
;; Transient menu for org-db v3 commands.

;;; Code:

(require 'transient)
(require 'org-db-v3-search)
(require 'org-db-v3-server)

;;;###autoload (autoload 'org-db-menu "org-db-v3-ui" nil t)
(transient-define-prefix org-db-menu ()
  "org-db v3 - Search and manage your org files."
  ["Search"
   ["Text Search"
    ("s" "Semantic search" org-db-v3-semantic-search)
    ("f" "Full-text search" org-db-v3-fulltext-search :if (lambda () nil))]  ;; TODO
   ["Structured"
    ("h" "Headlines" org-db-v3-headings :if (lambda () nil))  ;; TODO
    ("l" "Links" org-db-v3-links :if (lambda () nil))]]  ;; TODO
  ["Management"
   ("u" "Update current file" org-db-v3-update-current-file)
   ("S" "Server status" org-db-v3-server-status)
   ("q" "Quit" transient-quit-one)])

(defun org-db-v3-update-current-file ()
  "Manually update the current file."
  (interactive)
  (if (buffer-file-name)
      (progn
        (org-db-v3-index-file-async (buffer-file-name))
        (message "Indexing %s..." (buffer-file-name)))
    (message "No file associated with current buffer")))

(defun org-db-v3-server-status ()
  "Show server status."
  (interactive)
  (if (org-db-v3-server-running-p)
      (message "org-db server is running on %s" (org-db-v3-server-url))
    (message "org-db server is not running")))

(provide 'org-db-v3-ui)
;;; org-db-v3-ui.el ends here
```

**Step 3: Update main package**

Modify `elisp/org-db-v3.el` to add at the end:
```elisp
(require 'org-db-v3-ui)
(require 'org-db-v3-search)

;; Make org-db-menu available as M-x org-db
(defalias 'org-db 'org-db-menu)
```

**Step 4: Test interactively**

1. Restart Emacs or reload all files
2. Run: `M-x org-db-v3-enable`
3. Open and save a few org files
4. Wait for indexing to complete
5. Run: `M-x org-db` (or `M-x org-db-menu`)
6. Press `s` for semantic search
7. Enter a query like "machine learning"
8. Select a result

Expected: Opens file at correct location

**Step 5: Commit search UI**

```bash
git add elisp/
git commit -m "feat: add semantic search command and transient menu"
```

---

## Phase 9: CLIP Image Search

### Task 9.1: CLIP Service & Image Indexing

**Files:**
- Create: `python/org_db_server/services/clip.py`
- Create: `tests/test_clip.py`
- Modify: `python/org_db_server/api/indexing.py`

**Step 1: Write test for CLIP encoding**

Create `tests/test_clip.py`:
```python
"""Tests for CLIP service."""
import pytest
import numpy as np
from PIL import Image
from org_db_server.services.clip import CLIPService

@pytest.fixture
def clip_service():
    """Create CLIP service."""
    return CLIPService(model_name="openai/clip-vit-base-patch32")

@pytest.fixture
def sample_image(tmp_path):
    """Create a sample image for testing."""
    img = Image.new('RGB', (100, 100), color='red')
    img_path = tmp_path / "test.png"
    img.save(img_path)
    return str(img_path)

def test_encode_image(clip_service, sample_image):
    """Test encoding an image."""
    embedding = clip_service.encode_image(sample_image)

    assert isinstance(embedding, np.ndarray)
    assert embedding.shape[0] == 512  # CLIP dimension
    assert np.isfinite(embedding).all()

def test_encode_text(clip_service):
    """Test encoding text query."""
    embedding = clip_service.encode_text("a red square")

    assert isinstance(embedding, np.ndarray)
    assert embedding.shape[0] == 512
```

**Step 2: Run test to verify it fails**

Run: `cd python && uv run pytest tests/test_clip.py::test_encode_image -v`

Expected: FAIL with "No module named 'org_db_server.services.clip'"

**Step 3: Implement CLIP service**

Create `python/org_db_server/services/clip.py`:
```python
"""CLIP service for image embeddings."""
import numpy as np
from typing import Union, List
from PIL import Image
from transformers import CLIPProcessor, CLIPModel
import torch

class CLIPService:
    """Service for generating image and text embeddings using CLIP."""

    def __init__(self, model_name: str = "openai/clip-vit-base-patch32"):
        """Initialize CLIP model."""
        self.model_name = model_name
        self.model = CLIPModel.from_pretrained(model_name)
        self.processor = CLIPProcessor.from_pretrained(model_name)
        self.device = "cuda" if torch.cuda.is_available() else "cpu"
        self.model.to(self.device)
        self.dimension = self.model.config.projection_dim

    def encode_image(self, image_path: str) -> np.ndarray:
        """Encode an image to an embedding vector."""
        image = Image.open(image_path).convert("RGB")
        inputs = self.processor(images=image, return_tensors="pt")
        inputs = {k: v.to(self.device) for k, v in inputs.items()}

        with torch.no_grad():
            image_features = self.model.get_image_features(**inputs)
            # Normalize
            image_features = image_features / image_features.norm(dim=-1, keepdim=True)

        return image_features.cpu().numpy()[0]

    def encode_text(self, text: str) -> np.ndarray:
        """Encode text query to an embedding vector."""
        inputs = self.processor(text=text, return_tensors="pt", padding=True)
        inputs = {k: v.to(self.device) for k, v in inputs.items()}

        with torch.no_grad():
            text_features = self.model.get_text_features(**inputs)
            # Normalize
            text_features = text_features / text_features.norm(dim=-1, keepdim=True)

        return text_features.cpu().numpy()[0]

    def similarity(self, emb1: np.ndarray, emb2: np.ndarray) -> float:
        """Calculate cosine similarity between embeddings."""
        return float(np.dot(emb1, emb2))

# Global CLIP service instance
_clip_service = None

def get_clip_service(model_name: str = "openai/clip-vit-base-patch32") -> CLIPService:
    """Get or create the global CLIP service."""
    global _clip_service
    if _clip_service is None or _clip_service.model_name != model_name:
        _clip_service = CLIPService(model_name)
    return _clip_service
```

**Step 4: Run tests to verify they pass**

Run: `cd python && uv run pytest tests/test_clip.py -v`

Expected: PASS (may take time to download CLIP model)

**Step 5: Commit CLIP service**

```bash
git add python/org_db_server/services/clip.py tests/test_clip.py
git commit -m "feat: add CLIP service for image embeddings"
```

---

## Phase 10: Documentation & Polish

### Task 10.1: User Documentation

**Files:**
- Create: `README.org`
- Create: `docs/user-guide.org`
- Create: `docs/api.org`

**Step 1: Create main README**

Create `README.org`:
```org
#+TITLE: org-db v3
#+AUTHOR: Your Name

* Overview

org-db v3 is a powerful indexing and search system for org-mode files that combines:

- *Semantic Search*: Find content by meaning, not just keywords
- *Full-Text Search*: Traditional fast text search
- *Image Search*: Find images by description using CLIP
- *Structured Queries*: Search by headlines, tags, properties, links, etc.

** Architecture

org-db v3 uses a hybrid architecture:
- *Emacs*: Parses org files using org-element, provides UI
- *Python FastAPI*: Handles indexing, embeddings, vector search
- *libsql*: Database for structured data and embeddings

** Features

- Automatic background indexing when you save org files
- Non-blocking async operations - Emacs stays responsive
- Pluggable embedding models (local or API-based)
- CLIP-based image search
- Transient menu interface
- Compatible with existing org-db workflows

* Installation

** Prerequisites

- Emacs 28.1 or later
- Python 3.10 or later
- Poetry (Python package manager)

** Setup

1. Clone the repository:
   #+begin_src bash
   git clone https://github.com/yourusername/org-db-v3
   cd org-db-v3
   #+end_src

2. Run setup script:
   #+begin_src bash
   ./scripts/setup.sh
   #+end_src

3. Add to your Emacs config:
   #+begin_src elisp
   (add-to-list 'load-path "/path/to/org-db-v3/elisp")
   (require 'org-db-v3)
   (org-db-v3-enable)
   #+end_src

* Usage

** Quick Start

1. Open an org file - it will be automatically queued for indexing
2. Save the file - indexing happens in the background
3. Press ~M-x org-db~ to open the search menu
4. Press ~s~ for semantic search
5. Enter your query and select results

** Search Commands

- ~M-x org-db-semantic-search~ - Search by meaning
- ~M-x org-db~ - Main menu (transient)

** Server Management

- ~M-x org-db-v3-start-server~ - Manually start server
- ~M-x org-db-v3-stop-server~ - Stop server
- Server auto-starts by default when needed

* Configuration

#+begin_src elisp
;; Customize server settings
(setq org-db-v3-server-port 8765)
(setq org-db-v3-server-host "127.0.0.1")

;; Auto-start server
(setq org-db-v3-auto-start-server t)

;; Python command (use 'python3' if poetry not available)
(setq org-db-v3-python-command "poetry")
#+end_src

* Development

See [[file:docs/architecture.org][Architecture]] and [[file:docs/plans/2025-10-11-org-db-v3-implementation.md][Implementation Plan]].

* License

[Your chosen license]
```

**Step 2: Create user guide**

Create `docs/user-guide.org`:
```org
#+TITLE: org-db v3 User Guide

* Introduction

This guide covers everything you need to know to use org-db v3 effectively.

* Basic Concepts

** Automatic Indexing

When you:
1. Open an org file → Added to indexing queue
2. Save an org file → Re-indexed if changed
3. Wait for idle time → Queue processes in background

** Search Types

*** Semantic Search
Finds content by meaning using embeddings.

Example: Search "neural networks" finds:
- "deep learning"
- "machine learning models"
- "artificial intelligence"

*** Full-Text Search
Traditional keyword search (TODO: not yet implemented).

*** Structured Search
Search by org structure:
- Headlines
- Tags
- Properties
- TODO states

* Common Workflows

** Daily Note Taking

1. Take notes in org files as usual
2. Files auto-index when saved
3. Use semantic search to find related notes later

** Research Management

1. Keep papers/notes in org files
2. Tag and categorize with org headlines
3. Use semantic search to find relevant research
4. Use image search to find diagrams/figures

** Project Management

1. Track TODOs in org files
2. Search by TODO states
3. Find related tasks semantically
4. Link projects with org links

* Troubleshooting

** Server won't start

Check:
- Python 3.10+ installed?
- Poetry installed?
- Run ~./scripts/setup.sh~ again
- Check *org-db-server* buffer for errors

** Indexing not working

Check:
- Is server running? (~M-x org-db-v3-server-status~)
- Check queue: ~org-db-v3-index-queue~
- Manually index: ~M-x org-db-v3-update-current-file~

** Search returns no results

- Wait for indexing to complete
- Check database has data (server logs)
- Try different search terms

* Advanced Usage

** Custom Embedding Models

Edit ~python/org_db_server/config.py~:
#+begin_src python
default_embedding_model: str = "sentence-transformers/all-mpnet-base-v2"
#+end_src

** Remote Server

For team usage, deploy server remotely and configure:
#+begin_src elisp
(setq org-db-v3-server-host "your-server.com")
(setq org-db-v3-auto-start-server nil)  ; Don't auto-start
#+end_src
```

**Step 3: Commit documentation**

```bash
git add README.org docs/
git commit -m "docs: add user documentation and guides"
```

---

## Summary & Next Steps

This implementation plan provides a complete roadmap to build org-db v3 with:

✅ **Phase 1**: Project foundation and basic server
✅ **Phase 2**: Database schema with libsql
✅ **Phase 3**: Emacs org parsing to JSON
✅ **Phase 4**: File indexing API
✅ **Phase 5**: Async indexing with hooks
✅ **Phase 6**: Text chunking and embeddings
✅ **Phase 7**: Semantic search
✅ **Phase 8**: Emacs search UI
✅ **Phase 9**: CLIP image search
✅ **Phase 10**: Documentation

**Not yet covered (future work):**
- Full-text search (FTS5)
- Additional structured queries (contacts, locations, etc.)
- Audio/video transcription
- Server-Sent Events for progress
- More comprehensive testing
- Performance optimization
- Migration from v2

**Each task follows TDD principles:**
1. Write failing test
2. Implement minimal code
3. Verify tests pass
4. Commit

This keeps the implementation incremental, testable, and maintainable.
