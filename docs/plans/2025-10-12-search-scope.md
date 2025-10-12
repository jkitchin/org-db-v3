# Search Scope Implementation Plan

> **For Claude:** Use `${SUPERPOWERS_SKILLS_ROOT}/skills/collaboration/executing-plans/SKILL.md` to implement this plan task-by-task.

**Goal:** Add scoped search functionality to limit searches to directories, Projectile projects, or tags/keywords.

**Architecture:** Add transient submenu for scope selection, store scope state that applies to next search only, extend backend API to accept optional filename patterns and keyword filters, modify all search queries to respect filters.

**Tech Stack:** Emacs Lisp (transient), Python (FastAPI, SQLite), existing plz.el HTTP client

---

## Task 1: Add Scope State and Helper Functions (Elisp)

**Files:**
- Modify: `elisp/org-db-v3-ui.el`

**Step 1: Add scope state variable**

Add after the `require` statements at the top of `org-db-v3-ui.el`:

```elisp
(defvar org-db-v3-search-scope '(all . nil)
  "Current search scope. Format: (type . value)
   - (all . nil) - search all files
   - (directory . \"/path/to/dir/\") - files under directory
   - (project . \"/path/to/project/\") - files in project root
   - (tag . \"tag-name\") - files with specific keyword/tag
   Resets to (all . nil) after each search.")
```

**Step 2: Add scope description helper**

Add before the `org-db-menu` definition:

```elisp
(defun org-db-v3--scope-description ()
  "Return current scope description for transient header."
  (pcase (car org-db-v3-search-scope)
    ('all "All files")
    ('directory (format "Directory: %s"
                        (file-name-nondirectory
                         (directory-file-name (cdr org-db-v3-search-scope)))))
    ('project (format "Project: %s"
                      (file-name-nondirectory
                       (directory-file-name (cdr org-db-v3-search-scope)))))
    ('tag (format "Tag: %s" (cdr org-db-v3-search-scope)))
    (_ "All files")))
```

**Step 3: Add scope-to-params converter**

Add after the scope description helper:

```elisp
(defun org-db-v3--scope-to-params ()
  "Convert current scope to API filter parameters.
Returns plist with :filename_pattern and/or :keyword."
  (pcase (car org-db-v3-search-scope)
    ('all nil)
    ('directory
     (list :filename_pattern (concat (cdr org-db-v3-search-scope) "%")))
    ('project
     (list :filename_pattern (concat (cdr org-db-v3-search-scope) "%")))
    ('tag
     (list :keyword (cdr org-db-v3-search-scope)))
    (_ nil)))
```

**Step 4: Test helpers in *scratch***

```elisp
;; Test scope description
(let ((org-db-v3-search-scope '(directory . "/home/user/projects/")))
  (org-db-v3--scope-description))
;; Should return: "Directory: projects"

;; Test scope-to-params
(let ((org-db-v3-search-scope '(project . "/home/user/work/")))
  (org-db-v3--scope-to-params))
;; Should return: (:filename_pattern "/home/user/work/%")

(let ((org-db-v3-search-scope '(tag . "important")))
  (org-db-v3--scope-to-params))
;; Should return: (:keyword "important")
```

**Step 5: Commit**

```bash
git add elisp/org-db-v3-ui.el
git commit -m "feat: add scope state and helper functions"
```

---

## Task 2: Add Scope Setter Functions (Elisp)

**Files:**
- Modify: `elisp/org-db-v3-ui.el`

**Step 1: Add scope-all function**

Add after the helper functions:

```elisp
;;;###autoload
(defun org-db-v3-scope-all ()
  "Set search scope to all files."
  (interactive)
  (setq org-db-v3-search-scope '(all . nil))
  (message "Scope: All files (next search only)"))
```

**Step 2: Add scope-directory function**

```elisp
;;;###autoload
(defun org-db-v3-scope-directory ()
  "Set search scope to a directory."
  (interactive)
  (let ((dir (read-directory-name "Limit search to directory: ")))
    (when dir
      (setq org-db-v3-search-scope
            (cons 'directory (expand-file-name dir)))
      (message "Scope: %s (next search only)"
               (org-db-v3--scope-description)))))
```

**Step 3: Add scope-project function**

```elisp
;;;###autoload
(defun org-db-v3-scope-project ()
  "Set search scope to current Projectile project."
  (interactive)
  (if (and (fboundp 'projectile-project-root)
           (projectile-project-root))
      (progn
        (setq org-db-v3-search-scope
              (cons 'project (projectile-project-root)))
        (message "Scope: %s (next search only)"
                 (org-db-v3--scope-description)))
    (message "No project detected. Scope unchanged.")
    (ding)))
```

**Step 4: Add scope-tag function**

```elisp
;;;###autoload
(defun org-db-v3-scope-tag ()
  "Set search scope to files with a specific keyword/tag."
  (interactive)
  (let ((tag (read-string "Limit search to keyword/tag: ")))
    (when (and tag (not (string-empty-p tag)))
      (setq org-db-v3-search-scope
            (cons 'tag tag))
      (message "Scope: %s (next search only)"
               (org-db-v3--scope-description)))))
```

**Step 5: Test scope setters manually**

```elisp
M-x org-db-v3-scope-directory RET /tmp/ RET
;; Should show message: "Scope: Directory: tmp (next search only)"

M-x org-db-v3-scope-tag RET work RET
;; Should show message: "Scope: Tag: work (next search only)"

M-x org-db-v3-scope-all RET
;; Should show message: "Scope: All files (next search only)"
```

**Step 6: Commit**

```bash
git add elisp/org-db-v3-ui.el
git commit -m "feat: add scope setter functions"
```

---

## Task 3: Add Scope Submenu to Transient (Elisp)

**Files:**
- Modify: `elisp/org-db-v3-ui.el`

**Step 1: Define scope submenu**

Add before the main `org-db-menu` definition:

```elisp
;;;###autoload (autoload 'org-db-v3-scope-menu "org-db-v3-ui" nil t)
(transient-define-prefix org-db-v3-scope-menu ()
  "Set search scope for next search."
  ["Search Scope (applies to next search only)"
   ("a" "All files" org-db-v3-scope-all
    :description "Search all indexed files")
   ("d" "Directory..." org-db-v3-scope-directory
    :description "Limit to specific directory")
   ("p" "Current project" org-db-v3-scope-project
    :description "Limit to Projectile project")
   ("t" "Tag/Keyword..." org-db-v3-scope-tag
    :description "Limit to files with keyword")])
```

**Step 2: Update main menu to show scope**

Modify the `org-db-menu` transient to add scope indicator and submenu option. Change the first line:

```elisp
(transient-define-prefix org-db-menu ()
  "org-db v3 - Search and manage your org files."
  [:description (lambda ()
                  (format "org-db v3 [Scope: %s]"
                          (org-db-v3--scope-description)))
```

**Step 3: Add scope option to Options section**

In the main menu, modify the Options section to add the scope submenu:

```elisp
["Options"
 ("-s" "Set scope..." org-db-v3-scope-menu)
 ("q" "Quit" transient-quit-one)]
```

**Step 4: Test transient menu**

```elisp
M-x org-db-menu RET
;; Should show header: "org-db v3 [Scope: All files]"

;; Press -s to open scope submenu
;; Press d, enter /tmp/
;; Return to main menu
;; Header should show: "org-db v3 [Scope: Directory: tmp]"
```

**Step 5: Commit**

```bash
git add elisp/org-db-v3-ui.el
git commit -m "feat: add scope submenu to transient"
```

---

## Task 4: Update Search Functions to Use Scope (Elisp)

**Files:**
- Modify: `elisp/org-db-v3-search.el`

**Step 1: Update semantic search to use scope**

Find the `org-db-v3-semantic-search` function and modify the `plz` call to include scope params:

```elisp
(defun org-db-v3-semantic-search (query)
  "Search org files semantically using embeddings."
  (interactive "sSearch query: ")
  (org-db-v3-ensure-server)
  (let* ((scope-params (org-db-v3--scope-to-params))
         (request-body (append (list :query query
                                     :limit org-db-v3-search-default-limit)
                               scope-params)))
    (plz 'post (concat (org-db-v3-server-url) "/api/search/semantic")
      :headers '(("Content-Type" . "application/json"))
      :body (json-encode request-body)
      :as #'json-read
      :then (lambda (response)
              ;; Reset scope after search
              (setq org-db-v3-search-scope '(all . nil))
              (org-db-v3--display-semantic-results response))
      :else (lambda (error)
              ;; Reset scope even on error
              (setq org-db-v3-search-scope '(all . nil))
              (message "Search failed: %s" (plz-error-message error))))))
```

**Step 2: Update fulltext search to use scope**

Find the `org-db-v3-fulltext-search` function and apply the same pattern:

```elisp
(defun org-db-v3-fulltext-search (query)
  "Search org files using full-text search (FTS5)."
  (interactive "sSearch query: ")
  (org-db-v3-ensure-server)
  (let* ((scope-params (org-db-v3--scope-to-params))
         (request-body (append (list :query query
                                     :limit org-db-v3-search-default-limit)
                               scope-params)))
    (plz 'post (concat (org-db-v3-server-url) "/api/search/fulltext")
      :headers '(("Content-Type" . "application/json"))
      :body (json-encode request-body)
      :as #'json-read
      :then (lambda (response)
              ;; Reset scope after search
              (setq org-db-v3-search-scope '(all . nil))
              (org-db-v3--display-fulltext-results response))
      :else (lambda (error)
              ;; Reset scope even on error
              (setq org-db-v3-search-scope '(all . nil))
              (message "Search failed: %s" (plz-error-message error))))))
```

**Step 3: Update headline search to use scope**

Find the `org-db-v3-headline-search` function and apply the same pattern:

```elisp
(defun org-db-v3-headline-search (query)
  "Search org headlines."
  (interactive "sSearch headlines: ")
  (org-db-v3-ensure-server)
  (let* ((scope-params (org-db-v3--scope-to-params))
         (request-body (append (list :query query
                                     :limit org-db-v3-search-default-limit)
                               scope-params)))
    (plz 'post (concat (org-db-v3-server-url) "/api/search/headlines")
      :headers '(("Content-Type" . "application/json"))
      :body (json-encode request-body)
      :as #'json-read
      :then (lambda (response)
              ;; Reset scope after search
              (setq org-db-v3-search-scope '(all . nil))
              (org-db-v3--display-headline-results response))
      :else (lambda (error)
              ;; Reset scope even on error
              (setq org-db-v3-search-scope '(all . nil))
              (message "Search failed: %s" (plz-error-message error))))))
```

**Step 4: Update image search to use scope**

Find the `org-db-v3-image-search` function and apply the same pattern:

```elisp
(defun org-db-v3-image-search (query)
  "Search for images by text description using CLIP."
  (interactive "sSearch images: ")
  (org-db-v3-ensure-server)
  (let* ((scope-params (org-db-v3--scope-to-params))
         (request-body (append (list :query query
                                     :limit org-db-v3-search-default-limit)
                               scope-params)))
    (plz 'post (concat (org-db-v3-server-url) "/api/search/images")
      :headers '(("Content-Type" . "application/json"))
      :body (json-encode request-body)
      :as #'json-read
      :then (lambda (response)
              ;; Reset scope after search
              (setq org-db-v3-search-scope '(all . nil))
              (org-db-v3--display-image-results response))
      :else (lambda (error)
              ;; Reset scope even on error
              (setq org-db-v3-search-scope '(all . nil))
              (message "Search failed: %s" (plz-error-message error))))))
```

**Step 5: Commit**

```bash
git add elisp/org-db-v3-search.el
git commit -m "feat: update search functions to use scope"
```

---

## Task 5: Update Backend API Schemas (Python)

**Files:**
- Modify: `python/org_db_server/models/schemas.py`

**Step 1: Update SemanticSearchRequest**

Find the `SemanticSearchRequest` class and add optional filter fields:

```python
class SemanticSearchRequest(BaseModel):
    query: str
    limit: int = 10
    model: Optional[str] = None
    filename_pattern: Optional[str] = None  # SQL LIKE pattern for directory/project scope
    keyword: Optional[str] = None           # Keyword/tag filter
```

**Step 2: Update FulltextSearchRequest**

Find the `FulltextSearchRequest` class and add the same fields:

```python
class FulltextSearchRequest(BaseModel):
    query: str
    limit: int = 10
    filename_pattern: Optional[str] = None
    keyword: Optional[str] = None
```

**Step 3: Update HeadlineSearchRequest**

Find the `HeadlineSearchRequest` class and add the same fields:

```python
class HeadlineSearchRequest(BaseModel):
    query: str
    limit: int = 10
    filename_pattern: Optional[str] = None
    keyword: Optional[str] = None
```

**Step 4: Update ImageSearchRequest**

Find the `ImageSearchRequest` class and add the same fields:

```python
class ImageSearchRequest(BaseModel):
    query: str
    limit: int = 10
    filename_pattern: Optional[str] = None
    keyword: Optional[str] = None
```

**Step 5: Verify schemas can be imported**

```bash
cd python
uv run python -c "from org_db_server.models.schemas import SemanticSearchRequest; print('OK')"
```

Expected: `OK`

**Step 6: Commit**

```bash
git add python/org_db_server/models/schemas.py
git commit -m "feat: add scope filters to search request schemas"
```

---

## Task 6: Add Scope Filtering to Semantic Search (Python)

**Files:**
- Modify: `python/org_db_server/api/search.py`

**Step 1: Update semantic search endpoint**

Find the `@router.post("/semantic")` function and modify the query to add filters. Replace the query construction section:

```python
@router.post("/semantic", response_model=SemanticSearchResponse)
async def semantic_search(request: SemanticSearchRequest):
    """Search using semantic similarity."""
    try:
        # Generate query embedding
        embedding_service = get_embedding_service(request.model)
        query_embedding = embedding_service.generate_embeddings([request.query])[0]
        query_bytes = query_embedding.astype(np.float32).tobytes()

        cursor = db.conn.cursor()

        # Build base query
        base_query = """
            SELECT
                chunks.rowid as chunk_id,
                chunks.chunk_text,
                files.filename,
                chunks.chunk_type,
                chunks.begin_line,
                chunks.end_line,
                embeddings.embedding_vector
            FROM chunks
            INNER JOIN embeddings ON chunks.rowid = embeddings.chunk_id
            INNER JOIN files ON chunks.filename_id = files.rowid
        """

        params = []
        where_clauses = []

        # Add filename pattern filter if provided
        if request.filename_pattern:
            where_clauses.append("files.filename LIKE ?")
            params.append(request.filename_pattern)

        # Add keyword filter if provided
        if request.keyword:
            base_query += """
                INNER JOIN file_keywords ON files.rowid = file_keywords.filename_id
                INNER JOIN keywords ON file_keywords.keyword_id = keywords.rowid
            """
            where_clauses.append("keywords.keyword = ?")
            params.append(request.keyword)

        # Combine WHERE clauses
        if where_clauses:
            base_query += " WHERE " + " AND ".join(where_clauses)

        # Execute query
        cursor.execute(base_query, params)
        rows = cursor.fetchall()

        # Rest of the function remains the same (similarity calculation, sorting, etc.)
        # ... (keep existing code)
```

**Step 2: Test semantic search without filters**

```bash
curl -X POST http://127.0.0.1:8765/api/search/semantic \
  -H "Content-Type: application/json" \
  -d '{"query": "test", "limit": 5}'
```

Expected: Valid JSON response with results

**Step 3: Test semantic search with filename pattern**

```bash
# Adjust the pattern to match files in your database
curl -X POST http://127.0.0.1:8765/api/search/semantic \
  -H "Content-Type: application/json" \
  -d '{"query": "test", "limit": 5, "filename_pattern": "/Users/jkitchin/%"}'
```

Expected: Results only from matching paths

**Step 4: Test semantic search with keyword**

```bash
# Adjust keyword to one that exists in your database
curl -X POST http://127.0.0.1:8765/api/search/semantic \
  -H "Content-Type: application/json" \
  -d '{"query": "test", "limit": 5, "keyword": "TITLE"}'
```

Expected: Results only from files with that keyword

**Step 5: Commit**

```bash
git add python/org_db_server/api/search.py
git commit -m "feat: add scope filtering to semantic search"
```

---

## Task 7: Add Scope Filtering to Fulltext Search (Python)

**Files:**
- Modify: `python/org_db_server/api/search.py`

**Step 1: Update fulltext search endpoint**

Find the `@router.post("/fulltext")` function and apply the same filtering pattern:

```python
@router.post("/fulltext", response_model=FulltextSearchResponse)
async def fulltext_search(request: FulltextSearchRequest):
    """Search using full-text search (FTS5)."""
    try:
        cursor = db.conn.cursor()

        # Build base query
        base_query = """
            SELECT
                fts_content.filename,
                snippet(fts_content, 2, '<mark>', '</mark>', '...', 32) as snippet,
                offsets(fts_content) as offsets
            FROM fts_content
        """

        params = [request.query]
        where_clauses = ["fts_content MATCH ?"]

        # Add filename pattern filter if provided
        if request.filename_pattern:
            # Need to join with files table for filename pattern
            base_query = """
                SELECT
                    fts_content.filename,
                    snippet(fts_content, 2, '<mark>', '</mark>', '...', 32) as snippet,
                    offsets(fts_content) as offsets
                FROM fts_content
                INNER JOIN files ON fts_content.filename = files.filename
            """
            where_clauses.append("files.filename LIKE ?")
            params.append(request.filename_pattern)

        # Add keyword filter if provided
        if request.keyword:
            if "INNER JOIN files" not in base_query:
                base_query = base_query.replace(
                    "FROM fts_content",
                    "FROM fts_content INNER JOIN files ON fts_content.filename = files.filename"
                )
            base_query += """
                INNER JOIN file_keywords ON files.rowid = file_keywords.filename_id
                INNER JOIN keywords ON file_keywords.keyword_id = keywords.rowid
            """
            where_clauses.append("keywords.keyword = ?")
            params.append(request.keyword)

        # Combine WHERE clauses
        base_query += " WHERE " + " AND ".join(where_clauses)
        base_query += f" LIMIT {request.limit}"

        # Execute query
        cursor.execute(base_query, params)
        rows = cursor.fetchall()

        # Rest of the function remains the same
        # ... (keep existing code for processing results)
```

**Step 2: Test fulltext search with filters**

```bash
curl -X POST http://127.0.0.1:8765/api/search/fulltext \
  -H "Content-Type: application/json" \
  -d '{"query": "test", "limit": 5, "filename_pattern": "/Users/jkitchin/%"}'
```

Expected: Results only from matching paths

**Step 3: Commit**

```bash
git add python/org_db_server/api/search.py
git commit -m "feat: add scope filtering to fulltext search"
```

---

## Task 8: Add Scope Filtering to Headline Search (Python)

**Files:**
- Modify: `python/org_db_server/api/search.py`

**Step 1: Update headline search endpoint**

Find the `@router.post("/headlines")` function and apply filtering:

```python
@router.post("/headlines", response_model=HeadlineSearchResponse)
async def headline_search(request: HeadlineSearchRequest):
    """Search org headlines."""
    try:
        cursor = db.conn.cursor()

        # Build base query
        base_query = """
            SELECT
                headlines.title,
                headlines.level,
                headlines.todo_keyword,
                headlines.tags,
                files.filename,
                headlines.begin
            FROM headlines
            INNER JOIN files ON headlines.filename_id = files.rowid
        """

        params = [f"%{request.query}%"]
        where_clauses = ["headlines.title LIKE ?"]

        # Add filename pattern filter if provided
        if request.filename_pattern:
            where_clauses.append("files.filename LIKE ?")
            params.append(request.filename_pattern)

        # Add keyword filter if provided
        if request.keyword:
            base_query += """
                INNER JOIN file_keywords ON files.rowid = file_keywords.filename_id
                INNER JOIN keywords ON file_keywords.keyword_id = keywords.rowid
            """
            where_clauses.append("keywords.keyword = ?")
            params.append(request.keyword)

        # Combine WHERE clauses
        base_query += " WHERE " + " AND ".join(where_clauses)
        base_query += f" LIMIT {request.limit}"

        # Execute query
        cursor.execute(base_query, params)
        rows = cursor.fetchall()

        # Rest of the function remains the same
        # ... (keep existing code)
```

**Step 2: Test headline search with filters**

```bash
curl -X POST http://127.0.0.1:8765/api/search/headlines \
  -H "Content-Type: application/json" \
  -d '{"query": "test", "limit": 5, "filename_pattern": "/Users/jkitchin/%"}'
```

Expected: Headlines only from matching paths

**Step 3: Commit**

```bash
git add python/org_db_server/api/search.py
git commit -m "feat: add scope filtering to headline search"
```

---

## Task 9: Add Scope Filtering to Image Search (Python)

**Files:**
- Modify: `python/org_db_server/api/search.py`

**Step 1: Update image search endpoint**

Find the `@router.post("/images")` function and apply filtering:

```python
@router.post("/images", response_model=ImageSearchResponse)
async def image_search(request: ImageSearchRequest):
    """Search images by text description using CLIP."""
    try:
        # Generate query embedding
        clip_service = get_clip_service()
        query_embedding = clip_service.generate_text_embedding(request.query)
        query_bytes = query_embedding.astype(np.float32).tobytes()

        cursor = db.conn.cursor()

        # Build base query
        base_query = """
            SELECT
                images.rowid as image_id,
                images.image_path,
                files.filename,
                images.begin,
                image_embeddings.embedding_vector
            FROM images
            INNER JOIN image_embeddings ON images.rowid = image_embeddings.image_id
            INNER JOIN files ON images.filename_id = files.rowid
        """

        params = []
        where_clauses = []

        # Add filename pattern filter if provided
        if request.filename_pattern:
            where_clauses.append("files.filename LIKE ?")
            params.append(request.filename_pattern)

        # Add keyword filter if provided
        if request.keyword:
            base_query += """
                INNER JOIN file_keywords ON files.rowid = file_keywords.filename_id
                INNER JOIN keywords ON file_keywords.keyword_id = keywords.rowid
            """
            where_clauses.append("keywords.keyword = ?")
            params.append(request.keyword)

        # Combine WHERE clauses
        if where_clauses:
            base_query += " WHERE " + " AND ".join(where_clauses)

        # Execute query
        cursor.execute(base_query, params)
        rows = cursor.fetchall()

        # Rest of the function remains the same (similarity calculation, etc.)
        # ... (keep existing code)
```

**Step 2: Test image search with filters**

```bash
curl -X POST http://127.0.0.1:8765/api/search/images \
  -H "Content-Type: application/json" \
  -d '{"query": "diagram", "limit": 5, "filename_pattern": "/Users/jkitchin/%"}'
```

Expected: Images only from matching paths

**Step 3: Commit**

```bash
git add python/org_db_server/api/search.py
git commit -m "feat: add scope filtering to image search"
```

---

## Task 10: Integration Testing and Documentation

**Files:**
- Modify: `README.org`

**Step 1: Reload elisp code in Emacs**

```elisp
M-x load-file RET reload.el RET
M-x org-db-v3-reload RET
```

**Step 2: Test complete workflow**

1. Open transient menu: `M-x org-db-menu`
2. Press `-s` to open scope submenu
3. Press `d` and select a directory containing indexed org files
4. Return to main menu and verify scope shows in header
5. Press `v` for semantic search, enter a query
6. Verify results are only from the selected directory
7. Open menu again and verify scope reset to "All files"

**Step 3: Test with project scope**

1. Open a file in a Projectile project
2. Open menu, press `-s`, then `p` for project scope
3. Perform search, verify results are from project only
4. Verify scope resets after search

**Step 4: Test with tag scope**

1. Open menu, press `-s`, then `t` for tag scope
2. Enter a keyword that exists in your database (e.g., "TITLE")
3. Perform search, verify results are from files with that keyword
4. Verify scope resets after search

**Step 5: Update README.org**

Add to the "Usage" section after "Transient Menu":

```markdown
** Scoped Search

Limit searches to specific directories, projects, or tags:

*** Setting Scope

1. Open the menu: =M-x org-db= or =H-v=
2. Press =-s= to open the scope submenu
3. Choose scope type:
   - =a= :: All files (default)
   - =d= :: Directory (prompts for path)
   - =p= :: Current Projectile project
   - =t= :: Tag/Keyword (prompts for keyword)
4. Perform your search (semantic, fulltext, headlines, or images)
5. Scope automatically resets to "All files" after search

*** Scope Indicator

The current scope is displayed in the menu header:
- =org-db v3 [Scope: All files]= (default)
- =org-db v3 [Scope: Project: my-project]=
- =org-db v3 [Scope: Directory: documents]=
- =org-db v3 [Scope: Tag: work]=

*** How It Works

*Directory/Project scope*: Filters files by path prefix (e.g., =/home/user/project/=)

*Tag scope*: Filters files that have the specified keyword (from =#+KEYWORD:= lines)

*Applies once*: The scope applies only to the next search, then resets automatically.
```

**Step 6: Update Future Enhancements section**

Move the completed feature from "Future Enhancements" to "Completed Features":

```markdown
* Completed Features

- [X] Semantic search with embeddings
- [X] Full-text search (FTS5) with snippets
- [X] Image search with CLIP
- [X] Headline search
- [X] Agenda (TODOs, deadlines, scheduled)
- [X] Transient menu UI
- [X] Web interface with statistics
- [X] Non-blocking directory indexing
- [X] File browser (open files from database)
- [X] Search at point
- [X] Auto-indexing on save
- [X] Scoped search (directory, project, tag)
```

**Step 7: Commit**

```bash
git add README.org
git commit -m "docs: add scoped search documentation"
```

---

## Task 11: Final Testing and Error Handling

**Files:**
- Test: All search functions

**Step 1: Test error case - no files match scope**

1. Set scope to a directory with no indexed files
2. Perform a search
3. Verify: Empty results displayed gracefully
4. Verify: Scope still resets to "All files"

**Step 2: Test error case - projectile not available**

1. In a non-project directory, try to set project scope
2. Verify: Message "No project detected. Scope unchanged."
3. Verify: Scope remains at previous value

**Step 3: Test error case - empty tag**

1. Set tag scope but enter empty string
2. Verify: Scope unchanged (validation in `org-db-v3-scope-tag`)

**Step 4: Test scope persistence across menu closes**

1. Set scope to a directory
2. Close menu with `q`
3. Reopen menu
4. Verify: Scope still shows in header
5. Perform search
6. Reopen menu
7. Verify: Scope reset to "All files"

**Step 5: Test all search types with scope**

Test each search type with directory scope:
- Semantic search (v)
- Fulltext search (k)
- Headline search (h)
- Image search (i)
- Search at point (p)

Verify all work correctly and reset scope.

**Step 6: Commit if any fixes were needed**

```bash
# Only if you found and fixed bugs
git add <files>
git commit -m "fix: handle edge cases in scoped search"
```

---

## Implementation Complete

All tasks complete! The scoped search feature is now fully implemented:

✅ Elisp state management and helpers
✅ Scope setter functions
✅ Transient submenu with scope indicator
✅ Search functions updated to use scope
✅ Backend schemas extended
✅ All search endpoints support filtering
✅ Integration testing
✅ Documentation updated

**Key Files Modified:**
- `elisp/org-db-v3-ui.el` - Scope state, setters, transient menu
- `elisp/org-db-v3-search.el` - Search functions updated
- `python/org_db_server/models/schemas.py` - Request schemas
- `python/org_db_server/api/search.py` - All search endpoints
- `README.org` - Documentation

**Usage:**
1. Open menu: `M-x org-db` or `H-v`
2. Press `-s` to set scope
3. Choose: Directory (d), Project (p), Tag (t), or All (a)
4. Perform any search
5. Scope auto-resets after search
