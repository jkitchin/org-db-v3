;;; org-db-v3-search.el --- Semantic search for org-db v3 -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: org-db-v3
;; Keywords: org-mode, database, search

;;; Commentary:

;; Semantic search functionality for org-db v3.
;; Uses vector embeddings to find semantically similar content.

;;; Code:

(require 'json)
(require 'org)

;; Forward declarations
(declare-function org-db-v3-server-url "org-db-v3")
(declare-function org-db-v3-ensure-server "org-db-v3")
(declare-function org-db-v3--scope-to-params "org-db-v3-ui")

;; Forward declare scope variable
(defvar org-db-v3-search-scope)

;; Require plz only when available (not in tests)
(when (require 'plz nil t)
  (defvar plz-available t))

(unless (boundp 'org-db-v3-server-host)
  (defvar org-db-v3-server-host "127.0.0.1"))

(unless (boundp 'org-db-v3-server-port)
  (defvar org-db-v3-server-port 8765))

(unless (fboundp 'org-db-v3-server-url)
  (defun org-db-v3-server-url ()
    "Return the base URL for the org-db server."
    (format "http://%s:%d" org-db-v3-server-host org-db-v3-server-port)))

(unless (fboundp 'org-db-v3-ensure-server)
  (defun org-db-v3-ensure-server ()
    "Ensure server is running (stub for testing)."
    nil))

(defcustom org-db-v3-search-default-limit 10
  "Default number of search results to retrieve."
  :type 'integer
  :group 'org-db-v3)

(defcustom org-db-v3-search-use-reranking nil
  "Enable cross-encoder reranking for more accurate semantic search.
When enabled, retrieves more candidates and reranks them using a
cross-encoder model for better relevance. Slower but more accurate."
  :type 'boolean
  :group 'org-db-v3)

(defcustom org-db-v3-search-rerank-candidates 50
  "Number of candidates to retrieve before reranking.
Only used when `org-db-v3-search-use-reranking' is non-nil."
  :type 'integer
  :group 'org-db-v3)

;;;###autoload
(defun org-db-v3-semantic-search (query &optional limit)
  "Perform semantic search for QUERY.
Retrieve up to LIMIT results (default `org-db-v3-search-default-limit')."
  (interactive (list (read-string "Search query: ")
                    (when current-prefix-arg
                      (read-number "Limit: " org-db-v3-search-default-limit))))

  (org-db-v3-ensure-server)

  (let* ((limit (or limit org-db-v3-search-default-limit))
         (scope-params (when (fboundp 'org-db-v3--scope-to-params)
                         (org-db-v3--scope-to-params)))
         (request-body (append `((query . ,query)
                                (limit . ,limit)
                                (rerank . ,(if org-db-v3-search-use-reranking t :json-false))
                                (rerank_candidates . ,org-db-v3-search-rerank-candidates))
                              (when scope-params
                                (list (cons 'filename_pattern (plist-get scope-params :filename_pattern))
                                      (cons 'keyword (plist-get scope-params :keyword)))))))
    (plz 'post (concat (org-db-v3-server-url) "/api/search/semantic")
      :headers '(("Content-Type" . "application/json"))
      :body (json-encode (seq-filter (lambda (pair) (cdr pair)) request-body))
      :as #'json-read
      :then (lambda (response)
              ;; Reset scope after search
              (when (boundp 'org-db-v3-search-scope)
                (setq org-db-v3-search-scope '(all . nil)))
              (org-db-v3-display-search-results query response))
      :else (lambda (error)
              ;; Reset scope even on error
              (when (boundp 'org-db-v3-search-scope)
                (setq org-db-v3-search-scope '(all . nil)))
              (message "Search error: %s" (plz-error-message error))))))

(defun org-db-v3-display-search-results (query response)
  "Display search RESPONSE for QUERY using completing-read."
  (let* ((results (alist-get 'results response))
         (model-used (alist-get 'model_used response))
         (reranked (alist-get 'reranked response)))

    (if (zerop (length results))
        (message "No results found for: %s" query)

      ;; Build candidates with metadata
      (let* ((candidates nil)
             (metadata-table (make-hash-table :test 'equal)))

        (dotimes (i (length results))
          (let* ((result (aref results i))
                 (chunk-text (alist-get 'chunk_text result))
                 (filename (alist-get 'filename result))
                 (similarity (alist-get 'similarity_score result))
                 (begin-line (alist-get 'begin_line result))
                 (end-line (alist-get 'end_line result))
                 ;; Truncate and clean chunk text for display
                 (context-width 60)
                 (display-text (replace-regexp-in-string
                               "[\n\r]+" " "
                               (if (> (length chunk-text) context-width)
                                   (concat (substring chunk-text 0 (- context-width 3)) "...")
                                 chunk-text)))
                 ;; Pad context to fixed width for alignment
                 (padded-context (format (format "%%-%ds" context-width) display-text))
                 ;; Format with fixed-width columns: score | context | filename:line
                 (candidate (format "%-6.3f | %s | %s:%d"
                                   similarity
                                   padded-context
                                   filename
                                   begin-line)))

            ;; Store metadata
            (puthash candidate
                    (list :file filename
                          :line begin-line
                          :end-line end-line
                          :text chunk-text)
                    metadata-table)
            (push candidate candidates)))

        ;; Reverse to show best results first
        (setq candidates (nreverse candidates))

        ;; Let user select
        (let ((selection (completing-read
                         (format "Search results (%s%s, %d found): "
                                model-used
                                (if reranked " + reranked" "")
                                (length results))
                         candidates
                         nil t)))
          (when selection
            (let* ((metadata (gethash selection metadata-table))
                   (file (plist-get metadata :file))
                   (line (plist-get metadata :line)))
              (when (and file (file-exists-p file))
                (find-file file)
                (goto-char (point-min))
                (forward-line (1- line))
                (recenter)))))))))

;;;###autoload
(defun org-db-v3-search-at-point ()
  "Perform semantic search using text at point or region."
  (interactive)
  (let ((query (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (thing-at-point 'sentence t))))
    (if query
        (org-db-v3-semantic-search query)
      (message "No text found at point"))))

;;;###autoload
(defun org-db-v3-fulltext-search (query &optional limit)
  "Perform full-text search for QUERY using FTS5.
Retrieve up to LIMIT results (default `org-db-v3-search-default-limit')."
  (interactive (list (read-string "Fulltext search: ")
                    (when current-prefix-arg
                      (read-number "Limit: " org-db-v3-search-default-limit))))

  (org-db-v3-ensure-server)

  (let* ((limit (or limit org-db-v3-search-default-limit))
         (scope-params (when (fboundp 'org-db-v3--scope-to-params)
                         (org-db-v3--scope-to-params)))
         (request-body (append `((query . ,query)
                                (limit . ,limit))
                              (when scope-params
                                (list (cons 'filename_pattern (plist-get scope-params :filename_pattern))
                                      (cons 'keyword (plist-get scope-params :keyword)))))))
    (plz 'post (concat (org-db-v3-server-url) "/api/search/fulltext")
      :headers '(("Content-Type" . "application/json"))
      :body (json-encode (seq-filter (lambda (pair) (cdr pair)) request-body))
      :as #'json-read
      :then (lambda (response)
              ;; Reset scope after search
              (when (boundp 'org-db-v3-search-scope)
                (setq org-db-v3-search-scope '(all . nil)))
              (org-db-v3-display-fulltext-results query response))
      :else (lambda (error)
              ;; Reset scope even on error
              (when (boundp 'org-db-v3-search-scope)
                (setq org-db-v3-search-scope '(all . nil)))
              (message "Search error: %s" (plz-error-message error))))))

(defun org-db-v3-display-fulltext-results (query response)
  "Display full-text search RESPONSE for QUERY using completing-read."
  (let ((results (alist-get 'results response)))

    (if (zerop (length results))
        (message "No results found for: %s" query)

      ;; Build candidates with metadata
      (let* ((candidates nil)
             (metadata-table (make-hash-table :test 'equal)))

        (dotimes (i (length results))
          (let* ((result (aref results i))
                 (filename (alist-get 'filename result))
                 (title (alist-get 'title result))
                 (content (alist-get 'content result))
                 (tags (alist-get 'tags result))
                 (snippet (alist-get 'snippet result))
                 (rank (alist-get 'rank result))
                 ;; Extract search term from snippet (text between >>> and <<<)
                 (search-term (when (string-match ">>>\\([^<]+\\)<<<" snippet)
                               (match-string 1 snippet)))
                 ;; Clean snippet for display (remove markers and newlines)
                 (clean-snippet (replace-regexp-in-string
                                "[\n\r]+" " "
                                (replace-regexp-in-string ">>>\\|<<<" "" snippet)))
                 ;; Fixed widths for alignment
                 (snippet-width 60)
                 (display-snippet (if (> (length clean-snippet) snippet-width)
                                     (concat (substring clean-snippet 0 (- snippet-width 3)) "...")
                                   clean-snippet))
                 (padded-snippet (format (format "%%-%ds" snippet-width) display-snippet))
                 ;; Format with fixed-width columns: rank | snippet | filename
                 (candidate (format "%8.2f | %s | %s"
                                   (abs rank)  ; bm25 scores are negative
                                   padded-snippet
                                   filename)))

            ;; Store metadata
            (puthash candidate
                    (list :file filename
                          :title title
                          :content content
                          :tags tags
                          :snippet snippet
                          :search-term search-term
                          :rank rank)
                    metadata-table)
            (push candidate candidates)))

        ;; Keep order (already sorted by rank from server)
        (setq candidates (nreverse candidates))

        ;; Let user select
        (let ((selection (completing-read
                         (format "Fulltext results (%d found): " (length results))
                         candidates
                         nil t)))
          (when selection
            (let* ((metadata (gethash selection metadata-table))
                   (file (plist-get metadata :file))
                   (search-term (plist-get metadata :search-term)))
              (when (and file (file-exists-p file))
                (find-file file)
                (goto-char (point-min))
                ;; Try to search for the matched term first, then title
                (if (and search-term (search-forward search-term nil t))
                    (progn
                      (beginning-of-line)
                      (recenter))
                  (when (search-forward (plist-get metadata :title) nil t)
                    (beginning-of-line)
                    (recenter)))))))))))

;;;###autoload
(defun org-db-v3-image-search (query &optional limit)
  "Perform image search for QUERY using CLIP embeddings.
Retrieve up to LIMIT results (default `org-db-v3-search-default-limit')."
  (interactive (list (read-string "Image search query: ")
                    (when current-prefix-arg
                      (read-number "Limit: " org-db-v3-search-default-limit))))

  (org-db-v3-ensure-server)

  (let* ((limit (or limit org-db-v3-search-default-limit))
         (scope-params (when (fboundp 'org-db-v3--scope-to-params)
                         (org-db-v3--scope-to-params)))
         (request-body (append `((query . ,query)
                                (limit . ,limit))
                              (when scope-params
                                (list (cons 'filename_pattern (plist-get scope-params :filename_pattern))
                                      (cons 'keyword (plist-get scope-params :keyword)))))))
    (plz 'post (concat (org-db-v3-server-url) "/api/search/images")
      :headers '(("Content-Type" . "application/json"))
      :body (json-encode (seq-filter (lambda (pair) (cdr pair)) request-body))
      :as #'json-read
      :then (lambda (response)
              ;; Reset scope after search
              (when (boundp 'org-db-v3-search-scope)
                (setq org-db-v3-search-scope '(all . nil)))
              (org-db-v3-display-image-results query response))
      :else (lambda (error)
              ;; Reset scope even on error
              (when (boundp 'org-db-v3-search-scope)
                (setq org-db-v3-search-scope '(all . nil)))
              (message "Search error: %s" (plz-error-message error))))))

(defun org-db-v3-display-image-results (query response)
  "Display image search RESPONSE for QUERY using completing-read."
  (let* ((results (alist-get 'results response))
         (model-used (alist-get 'model_used response)))

    (if (zerop (length results))
        (message "No images found for: %s" query)

      ;; Build candidates with metadata
      (let* ((candidates nil)
             (metadata-table (make-hash-table :test 'equal)))

        (dotimes (i (length results))
          (let* ((result (aref results i))
                 (image-path (alist-get 'image_path result))
                 (filename (alist-get 'filename result))
                 (similarity (alist-get 'similarity_score result))
                 ;; Fixed widths for alignment
                 (image-width 40)
                 (display-image (file-name-nondirectory image-path))
                 (truncated-image (if (> (length display-image) image-width)
                                     (concat (substring display-image 0 (- image-width 3)) "...")
                                   display-image))
                 (padded-image (format (format "%%-%ds" image-width) truncated-image))
                 ;; Format with fixed-width columns: score | image-filename | org-filename
                 (text-part (format "%-6.3f | %s | %s"
                                    similarity
                                    padded-image
                                    filename))
                 ;; Add thumbnail if image exists
                 (candidate (if (and image-path (file-exists-p image-path))
                               (concat text-part
                                       "\n"
                                       (propertize " " 'display
                                                  (create-image image-path nil nil :width 200)))
                             text-part)))

            ;; Store metadata
            (puthash candidate
                     (list :image-path image-path
                           :file filename
                           :similarity similarity)
                     metadata-table)
            (push candidate candidates)))

        ;; Reverse to show best results first
        (setq candidates (nreverse candidates))

        ;; Let user select
        (let ((selection (completing-read
                          (format "Image results (%s, %d found): "
                                  model-used (length results))
                          candidates
                          nil t)))
          (when selection
            (let* ((metadata (gethash selection metadata-table))
                   (image-path (plist-get metadata :image-path))
                   (file (plist-get metadata :file)))
              ;; Open the org file
              (when (and file (file-exists-p file))
                (find-file file)
                ;; Try to search for the image link in the file
                (goto-char (point-min))
                (when (search-forward (file-name-nondirectory image-path) nil t)
                  (beginning-of-line)
                  (recenter))))))))))

;;;###autoload
(defun org-db-v3-headline-search ()
  "Browse all headlines and jump to selection.
You can filter candidates dynamically using completing-read."
  (interactive)

  (org-db-v3-ensure-server)

  (let* ((scope-params (when (fboundp 'org-db-v3--scope-to-params)
                         (org-db-v3--scope-to-params)))
         (request-body (append `((query . "")
                                (limit . 1000))
                              (when scope-params
                                (list (cons 'filename_pattern (plist-get scope-params :filename_pattern))
                                      (cons 'keyword (plist-get scope-params :keyword)))))))
    (plz 'post (concat (org-db-v3-server-url) "/api/search/headlines")
      :headers '(("Content-Type" . "application/json"))
      :body (json-encode (seq-filter (lambda (pair) (cdr pair)) request-body))
      :as #'json-read
      :then (lambda (response)
              ;; Reset scope after search
              (when (boundp 'org-db-v3-search-scope)
                (setq org-db-v3-search-scope '(all . nil)))
              (org-db-v3-display-headline-results response))
      :else (lambda (error)
              ;; Reset scope even on error
              (when (boundp 'org-db-v3-search-scope)
                (setq org-db-v3-search-scope '(all . nil)))
              (message "Search error: %s" (plz-error-message error))))))

(defun org-db-v3-display-headline-results (response)
  "Display headline search RESPONSE using completing-read."
  (let ((results (alist-get 'results response)))

    (if (zerop (length results))
        (message "No headlines found in database")

      ;; Build candidates with metadata
      (let* ((candidates nil)
             (metadata-table (make-hash-table :test 'equal)))

        (dotimes (i (length results))
          (let* ((result (aref results i))
                 (title (alist-get 'title result))
                 (filename (alist-get 'filename result))
                 (begin (alist-get 'begin result))
                 (level (alist-get 'level result))
                 ;; Fixed widths for alignment
                 (title-width 40)
                 (display-title (if (> (length title) title-width)
                                   (concat (substring title 0 (- title-width 3)) "...")
                                 title))
                 (padded-title (format (format "%%-%ds" title-width) display-title))
                 ;; Calculate line number from character position by reading file
                 (line-number (org-db-v3--char-to-line filename begin))
                 ;; Format with fixed-width columns: headline | filename:line
                 (candidate (format "%s | %s:%d"
                                   padded-title
                                   filename
                                   line-number)))

            ;; Store metadata
            (puthash candidate
                    (list :file filename
                          :begin begin
                          :line line-number
                          :level level)
                    metadata-table)
            (push candidate candidates)))

        ;; Keep original order (by filename and position)
        (setq candidates (nreverse candidates))

        ;; Let user select
        (let ((selection (completing-read
                         (format "Headlines (%d found): " (length results))
                         candidates
                         nil t)))
          (when selection
            (let* ((metadata (gethash selection metadata-table))
                   (file (plist-get metadata :file))
                   (begin (plist-get metadata :begin)))
              (when (and file (file-exists-p file))
                (find-file file)
                (goto-char begin)
                (org-show-entry)
                (recenter)))))))))

(defun org-db-v3--char-to-line (filename char-pos)
  "Convert character position CHAR-POS in FILENAME to line number.
Returns 1 if file doesn't exist."
  (if (file-exists-p filename)
      (with-temp-buffer
        (insert-file-contents filename)
        (goto-char (min char-pos (point-max)))
        (line-number-at-pos))
    ;; File doesn't exist (e.g., test files), return reasonable default
    1))

;;;###autoload
(defun org-db-v3-open-file ()
  "Browse all files in database and open selected file."
  (interactive)

  (org-db-v3-ensure-server)

  (plz 'get (concat (org-db-v3-server-url) "/api/stats/files")
    :as #'json-read
    :then (lambda (response)
            (org-db-v3-display-file-list response))
    :else (lambda (error)
            (message "Error fetching files: %s" (plz-error-message error)))))

(defun org-db-v3-display-file-list (response)
  "Display file list from RESPONSE using completing-read."
  (let* ((files (alist-get 'files response))
         (count (alist-get 'count response)))

    (if (zerop count)
        (message "No files found in database")

      ;; Build candidates with metadata
      (let* ((candidates nil)
             (metadata-table (make-hash-table :test 'equal)))

        (dotimes (i (length files))
          (let* ((file-info (aref files i))
                 (filename (alist-get 'filename file-info))
                 (indexed-at (alist-get 'indexed_at file-info))
                 ;; Format timestamp for display (remove microseconds if present)
                 (display-time (if indexed-at
                                  (replace-regexp-in-string "\\..*" "" indexed-at)
                                "unknown"))
                 ;; Format: timestamp | filename
                 (candidate (format "%s | %s"
                                   display-time
                                   filename)))

            ;; Store metadata
            (puthash candidate filename metadata-table)
            (push candidate candidates)))

        ;; Keep chronological order (most recent first, already sorted from server)
        (setq candidates (nreverse candidates))

        ;; Let user select
        (let ((selection (completing-read
                         (format "Open file (%d files in database): " count)
                         candidates
                         nil t)))
          (when selection
            (let ((file (gethash selection metadata-table)))
              (if (and file (file-exists-p file))
                  (find-file file)
                (message "File does not exist: %s" file)))))))))

(provide 'org-db-v3-search)
;;; org-db-v3-search.el ends here
