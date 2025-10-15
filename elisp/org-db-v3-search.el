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
                 (linked-file-path (alist-get 'linked_file_path result))
                 (linked-file-type (alist-get 'linked_file_type result))
                 ;; Truncate and clean chunk text for display
                 (context-width 60)
                 (display-text (replace-regexp-in-string
                               "[\n\r]+" " "
                               (if (> (length chunk-text) context-width)
                                   (concat (substring chunk-text 0 (- context-width 3)) "...")
                                 chunk-text)))
                 ;; Pad context to fixed width for alignment
                 (padded-context (format (format "%%-%ds" context-width) display-text))
                 ;; Note: File type prefix is now added by the Python API in chunk-text
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
                          :text chunk-text
                          :linked-file-path linked-file-path
                          :linked-file-type linked-file-type)
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

;; Ivy-based semantic search with dynamic collection
;;
;; The ivy-based semantic search queries the API as you type, providing a truly
;; dynamic search experience. Since semantic search uses embeddings, there's a
;; slight delay (20-50ms) as you type, but results are fast with libsql vector search.
;;
;; Key features:
;; - Queries API dynamically as you type (minimum 2 characters)
;; - Caches results to avoid redundant API calls
;; - Optional reranking for improved relevance
;; - Multiple actions (use M-o in ivy to access):
;;   - o: Open file at match location (default)
;;   - c: Copy chunk text to kill ring
;; - Results show similarity score, context snippet, and file location
;;
;; Usage: M-x org-db-v3-semantic-search-ivy RET
;; Then start typing your query - results update as you type
;; With prefix arg (C-u): Change the number of results per query

(defcustom org-db-v3-ivy-semantic-limit 20
  "Number of results to fetch per query for ivy-based semantic search.
Used when querying the API dynamically as you type."
  :type 'integer
  :group 'org-db-v3)

(defcustom org-db-v3-ivy-semantic-min-query-length 2
  "Minimum query length before searching semantically.
Queries shorter than this will show a prompt message."
  :type 'integer
  :group 'org-db-v3)

(defvar org-db-v3--semantic-cache (make-hash-table :test 'equal)
  "Cache for semantic search results to avoid redundant API calls.")

(defvar org-db-v3--current-semantic-limit 20
  "Current limit for semantic search, can be set via prefix arg.")

;;;###autoload
(defun org-db-v3-semantic-search-ivy (&optional limit)
  "Perform dynamic semantic search using ivy - queries API as you type.
Start typing to search - results update dynamically using vector embeddings.
LIMIT sets number of results per query (default `org-db-v3-ivy-semantic-limit').
With prefix arg (C-u), prompts for custom limit."
  (interactive (list (when current-prefix-arg
                       (read-number "Results per query: " org-db-v3-ivy-semantic-limit))))

  (org-db-v3-ensure-server)

  ;; Clear cache on new search
  (clrhash org-db-v3--semantic-cache)

  ;; Set current limit for use in dynamic collection
  (setq org-db-v3--current-semantic-limit (or limit org-db-v3-ivy-semantic-limit))

  (if (fboundp 'ivy-read)
      (ivy-read "Semantic search (dynamic): "
                #'org-db-v3--dynamic-semantic-collection
                :dynamic-collection t
                :caller 'org-db-v3-semantic-search-ivy
                :action '(1
                         ("o" org-db-v3--open-semantic-candidate "Open file")
                         ("c" org-db-v3--copy-semantic-text "Copy text")))
    (user-error "Ivy is required for dynamic semantic search. Use org-db-v3-semantic-search instead")))

(defun org-db-v3--dynamic-semantic-collection (input)
  "Fetch semantic results matching INPUT dynamically.
Called by ivy as the user types. Returns a list of candidates.
Results are cached to avoid redundant API calls."
  (if (< (length input) org-db-v3-ivy-semantic-min-query-length)
      ;; Show prompt for short queries
      (list (format "Type at least %d characters to search..." org-db-v3-ivy-semantic-min-query-length))

    ;; Check cache first
    (or (gethash input org-db-v3--semantic-cache)
        ;; Not in cache, fetch from API
        (let* ((scope-params (when (fboundp 'org-db-v3--scope-to-params)
                               (org-db-v3--scope-to-params)))
               (request-body (append `((query . ,input)
                                      (limit . ,org-db-v3--current-semantic-limit)
                                      (rerank . ,(if org-db-v3-search-use-reranking t :json-false))
                                      (rerank_candidates . ,org-db-v3-search-rerank-candidates))
                                    (when scope-params
                                      (list (cons 'filename_pattern (plist-get scope-params :filename_pattern))
                                            (cons 'keyword (plist-get scope-params :keyword))))))
               ;; Synchronous request (required for dynamic collection)
               (response
                (condition-case err
                    (let ((url-request-method "POST")
                          (url-request-extra-headers '(("Content-Type" . "application/json")))
                          (url-request-data (encode-coding-string
                                            (json-encode (seq-filter (lambda (pair) (cdr pair)) request-body))
                                            'utf-8)))
                      (let ((buffer (url-retrieve-synchronously
                                     (concat (org-db-v3-server-url) "/api/search/semantic")
                                     t nil 5)))  ; 5 second timeout
                        (if (not buffer)
                            nil
                          (unwind-protect
                              (with-current-buffer buffer
                                (goto-char (point-min))
                                (when (re-search-forward "^$" nil t)
                                  (json-read)))
                            (kill-buffer buffer)))))
                  (error
                   (message "Semantic search error: %S" err)
                   nil))))

          (if (and response (vectorp (alist-get 'results response)))
              (let* ((results (alist-get 'results response))
                     (candidates (if (zerop (length results))
                                    (list (format "No results found for: %s" input))
                                  (org-db-v3--build-ivy-semantic-candidates results))))
                ;; Cache the results
                (puthash input candidates org-db-v3--semantic-cache)
                candidates)
            ;; Error or no results
            (list (format "Search failed or no results for: %s" input)))))))

(defun org-db-v3--build-ivy-semantic-candidates (results)
  "Build ivy candidates from semantic RESULTS array.
Each candidate is a string with metadata stored as text properties."
  (let ((candidates nil))
    (dotimes (i (length results))
      (let* ((result (aref results i))
             (chunk-text (alist-get 'chunk_text result))
             (filename (alist-get 'filename result))
             (similarity (alist-get 'similarity_score result))
             (begin-line (alist-get 'begin_line result))
             (end-line (alist-get 'end_line result))
             (linked-file-path (alist-get 'linked_file_path result))
             (linked-file-type (alist-get 'linked_file_type result))
             ;; Truncate and clean chunk text for display
             (context-width 60)
             (display-text (replace-regexp-in-string
                           "[\n\r]+" " "
                           (if (> (length chunk-text) context-width)
                               (concat (substring chunk-text 0 (- context-width 3)) "...")
                             chunk-text)))
             (padded-context (format (format "%%-%ds" context-width) display-text))
             ;; Format: score | context | filename:line
             (candidate (format "%-6.3f | %s | %s:%d"
                               similarity
                               padded-context
                               (file-name-nondirectory filename)
                               begin-line)))

        ;; Store metadata as text properties
        (put-text-property 0 (length candidate) 'semantic-data
                          `(:file ,filename
                            :line ,begin-line
                            :end-line ,end-line
                            :text ,chunk-text
                            :linked-file-path ,linked-file-path
                            :linked-file-type ,linked-file-type)
                          candidate)
        (push candidate candidates)))
    (nreverse candidates)))

(defun org-db-v3--open-semantic-candidate (candidate)
  "Open file for selected semantic CANDIDATE.
CANDIDATE is a string with metadata stored in text properties."
  (let* ((data (get-text-property 0 'semantic-data candidate))
         (file (plist-get data :file))
         (line (plist-get data :line)))
    (when (and file (file-exists-p file))
      (find-file file)
      (goto-char (point-min))
      (forward-line (1- line))
      (recenter))))

(defun org-db-v3--copy-semantic-text (candidate)
  "Copy the chunk text to kill ring for CANDIDATE.
CANDIDATE is a string with metadata stored in text properties."
  (let* ((data (get-text-property 0 'semantic-data candidate))
         (text (plist-get data :text)))
    (when text
      (kill-new text)
      (message "Copied to kill ring: %s" (substring text 0 (min 50 (length text)))))))

;; Configure ivy for semantic search if available
(with-eval-after-load 'ivy
  (ivy-configure 'org-db-v3-semantic-search-ivy
    :height 15
    :sort-fn nil))  ; Keep relevance order from API

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

;; Ivy-based image search with dynamic collection
;;
;; The ivy-based image search queries the API as you type, providing a truly
;; dynamic search experience. Key features:
;;
;; - Queries API dynamically as you type (minimum 2 characters)
;; - Caches results to avoid redundant API calls
;; - Multi-line display showing both text and thumbnails
;; - Multiple actions (use M-o in ivy to access):
;;   - o: Open org file at image link location (default)
;;   - i: Open the image file directly
;;   - c: Copy image path to kill ring
;; - Thumbnails displayed at 400x250 max size
;; - Height configured for 20 lines to accommodate thumbnails
;;
;; Usage: M-x org-db-v3-image-search-ivy RET
;; Then start typing your query - results update as you type
;; With prefix arg (C-u): Change the number of results per query

(defcustom org-db-v3-ivy-image-limit 20
  "Number of images to fetch per query for ivy-based image search.
Used when querying the API dynamically as you type."
  :type 'integer
  :group 'org-db-v3)

(defcustom org-db-v3-ivy-min-query-length 2
  "Minimum query length before searching for images.
Queries shorter than this will show a prompt message."
  :type 'integer
  :group 'org-db-v3)

(defvar org-db-v3--image-cache (make-hash-table :test 'equal)
  "Cache for image search results to avoid redundant API calls.")

(defvar org-db-v3--current-image-limit 20
  "Current limit for image search, can be set via prefix arg.")

;;;###autoload
(defun org-db-v3-image-search-ivy (&optional limit)
  "Perform dynamic image search using ivy - queries API as you type.
Start typing to search - results update dynamically.
LIMIT sets number of results per query (default `org-db-v3-ivy-image-limit').
With prefix arg (C-u), prompts for custom limit."
  (interactive (list (when current-prefix-arg
                       (read-number "Results per query: " org-db-v3-ivy-image-limit))))

  (org-db-v3-ensure-server)

  ;; Clear cache on new search
  (clrhash org-db-v3--image-cache)

  ;; Set current limit for use in dynamic collection
  (setq org-db-v3--current-image-limit (or limit org-db-v3-ivy-image-limit))

  (if (fboundp 'ivy-read)
      (ivy-read "Image search (dynamic): "
                #'org-db-v3--dynamic-image-collection
                :dynamic-collection t
                :caller 'org-db-v3-image-search-ivy
                :action '(1
                         ("o" org-db-v3--open-image-candidate "Open org file")
                         ("i" org-db-v3--open-image-file "Open image file")
                         ("c" org-db-v3--copy-image-path "Copy image path")))
    (user-error "Ivy is required for dynamic image search. Use org-db-v3-image-search instead")))

(defun org-db-v3--dynamic-image-collection (input)
  "Fetch images matching INPUT dynamically.
Called by ivy as the user types. Returns a list of candidates with thumbnails.
Results are cached to avoid redundant API calls."
  (if (< (length input) org-db-v3-ivy-min-query-length)
      ;; Show prompt for short queries
      (list (format "Type at least %d characters to search..." org-db-v3-ivy-min-query-length))

    ;; Check cache first
    (or (gethash input org-db-v3--image-cache)
        ;; Not in cache, fetch from API
        (let* ((scope-params (when (fboundp 'org-db-v3--scope-to-params)
                               (org-db-v3--scope-to-params)))
               (request-body (append `((query . ,input)
                                      (limit . ,org-db-v3--current-image-limit))
                                    (when scope-params
                                      (list (cons 'filename_pattern (plist-get scope-params :filename_pattern))
                                            (cons 'keyword (plist-get scope-params :keyword))))))
               ;; Synchronous request (required for dynamic collection)
               (response
                (condition-case err
                    (let ((url-request-method "POST")
                          (url-request-extra-headers '(("Content-Type" . "application/json")))
                          (url-request-data (encode-coding-string
                                            (json-encode (seq-filter (lambda (pair) (cdr pair)) request-body))
                                            'utf-8)))
                      (with-current-buffer
                          (url-retrieve-synchronously
                           (concat (org-db-v3-server-url) "/api/search/images")
                           t nil 5)  ; 5 second timeout
                        (goto-char (point-min))
                        (re-search-forward "^$")
                        (json-read)))
                  (error
                   (message "Image search error: %S" err)
                   nil))))

          (if (and response (vectorp (alist-get 'results response)))
              (let* ((results (alist-get 'results response))
                     (candidates (if (zerop (length results))
                                    (list (format "No images found for: %s" input))
                                  (org-db-v3--build-ivy-image-candidates results))))
                ;; Cache the results
                (puthash input candidates org-db-v3--image-cache)
                candidates)
            ;; Error or no results
            (list (format "Search failed or no results for: %s" input)))))))

(defun org-db-v3--build-ivy-image-candidates (results)
  "Build ivy candidates with thumbnails from RESULTS array.
Each candidate is a string with metadata stored as text properties."
  (let ((candidates nil))
    (dotimes (i (length results))
      (let* ((result (aref results i))
             (image-path (alist-get 'image_path result))
             (filename (alist-get 'filename result))
             (similarity (alist-get 'similarity_score result))
             ;; Text line - compact format for filtering
             (image-basename (file-name-nondirectory image-path))
             (org-basename (file-name-nondirectory filename))
             (text-line (format "%.3f | %-40s | %s"
                               similarity
                               (if (> (length image-basename) 40)
                                   (concat (substring image-basename 0 37) "...")
                                 image-basename)
                               org-basename))
             ;; Candidate with thumbnail on second line
             (candidate (if (and image-path (file-exists-p image-path))
                           (concat text-line "\n"
                                  (propertize " " 'display
                                            (create-image image-path nil nil
                                                         :max-width 400
                                                         :max-height 250)))
                         text-line)))

        ;; Store metadata as text properties
        (put-text-property 0 (length candidate) 'image-data
                          `(:path ,image-path :file ,filename :score ,similarity)
                          candidate)
        (push candidate candidates)))
    (nreverse candidates)))

(defun org-db-v3--open-image-candidate (candidate)
  "Open org file for selected image CANDIDATE.
CANDIDATE is a string with metadata stored in text properties."
  (let* ((data (get-text-property 0 'image-data candidate))
         (file (plist-get data :file))
         (image-path (plist-get data :path)))
    (when (and file (file-exists-p file))
      (find-file file)
      (goto-char (point-min))
      (when (search-forward (file-name-nondirectory image-path) nil t)
        (beginning-of-line)
        (recenter)))))

(defun org-db-v3--open-image-file (candidate)
  "Open the image file directly for CANDIDATE.
CANDIDATE is a string with metadata stored in text properties."
  (let* ((data (get-text-property 0 'image-data candidate))
         (image-path (plist-get data :path)))
    (when (and image-path (file-exists-p image-path))
      (find-file image-path))))

(defun org-db-v3--copy-image-path (candidate)
  "Copy the image file path to kill ring for CANDIDATE.
CANDIDATE is a string with metadata stored in text properties."
  (let* ((data (get-text-property 0 'image-data candidate))
         (image-path (plist-get data :path)))
    (when image-path
      (kill-new image-path)
      (message "Copied to kill ring: %s" image-path))))

;; Configure ivy for image search if available
(with-eval-after-load 'ivy
  (ivy-configure 'org-db-v3-image-search-ivy
    :height 20  ; More space for thumbnails
    :sort-fn nil))  ; Keep relevance order from API


;; Ivy-based fulltext search with dynamic collection
;;
;; The ivy-based fulltext search queries the FTS5 index as you type, providing
;; dynamic results. FTS5 is fast (~10-100ms) so this provides good UX.

(defcustom org-db-v3-ivy-fulltext-limit 20
  "Number of results to fetch per query for ivy-based fulltext search.
Used when querying the API dynamically as you type."
  :type 'integer
  :group 'org-db-v3)

(defcustom org-db-v3-ivy-fulltext-min-query-length 2
  "Minimum query length before searching fulltext.
Queries shorter than this will show a prompt message."
  :type 'integer
  :group 'org-db-v3)

(defvar org-db-v3--fulltext-cache (make-hash-table :test 'equal)
  "Cache for fulltext search results to avoid redundant API calls.")

(defvar org-db-v3--current-fulltext-limit 20
  "Current limit for fulltext search, can be set via prefix arg.")

;;;###autoload
(defun org-db-v3-fulltext-search-ivy (&optional limit)
  "Perform dynamic fulltext search using ivy - queries API as you type.
Start typing to search - results update dynamically.
LIMIT sets number of results per query (default `org-db-v3-ivy-fulltext-limit').
With prefix arg (C-u), prompts for custom limit."
  (interactive (list (when current-prefix-arg
                       (read-number "Results per query: " org-db-v3-ivy-fulltext-limit))))

  (org-db-v3-ensure-server)

  ;; Clear cache on new search
  (clrhash org-db-v3--fulltext-cache)

  ;; Set current limit for use in dynamic collection
  (setq org-db-v3--current-fulltext-limit (or limit org-db-v3-ivy-fulltext-limit))

  (if (fboundp 'ivy-read)
      (ivy-read "Fulltext search (dynamic): "
                #'org-db-v3--dynamic-fulltext-collection
                :dynamic-collection t
                :caller 'org-db-v3-fulltext-search-ivy
                :action #'org-db-v3--open-fulltext-candidate)
    (user-error "Ivy is required for dynamic fulltext search. Use org-db-v3-fulltext-search instead")))

(defun org-db-v3--dynamic-fulltext-collection (input)
  "Fetch fulltext results matching INPUT dynamically.
Called by ivy as the user types. Returns a list of candidates.
Results are cached to avoid redundant API calls."
  (if (< (length input) org-db-v3-ivy-fulltext-min-query-length)
      ;; Show prompt for short queries
      (list (format "Type at least %d characters to search..." org-db-v3-ivy-fulltext-min-query-length))

    ;; Check cache first
    (or (gethash input org-db-v3--fulltext-cache)
        ;; Not in cache, fetch from API
        (let* ((scope-params (when (fboundp 'org-db-v3--scope-to-params)
                               (org-db-v3--scope-to-params)))
               (request-body (append `((query . ,input)
                                      (limit . ,org-db-v3--current-fulltext-limit))
                                    (when scope-params
                                      (list (cons 'filename_pattern (plist-get scope-params :filename_pattern))
                                            (cons 'keyword (plist-get scope-params :keyword))))))
               ;; Synchronous request (required for dynamic collection)
               (response
                (condition-case err
                    (let ((url-request-method "POST")
                          (url-request-extra-headers '(("Content-Type" . "application/json")))
                          (url-request-data (encode-coding-string
                                            (json-encode (seq-filter (lambda (pair) (cdr pair)) request-body))
                                            'utf-8)))
                      (let ((buffer (url-retrieve-synchronously
                                     (concat (org-db-v3-server-url) "/api/search/fulltext")
                                     t nil 5)))  ; 5 second timeout (FTS5 is fast)
                        (if (not buffer)
                            nil
                          (unwind-protect
                              (with-current-buffer buffer
                                (goto-char (point-min))
                                (when (re-search-forward "^$" nil t)
                                  (json-read)))
                            (kill-buffer buffer)))))
                  (error
                   (message "Fulltext search error: %S" err)
                   nil))))

          (if (and response (vectorp (alist-get 'results response)))
              (let* ((results (alist-get 'results response))
                     (candidates (if (zerop (length results))
                                    (list (format "No results found for: %s" input))
                                  (org-db-v3--build-ivy-fulltext-candidates results))))
                ;; Cache the results
                (puthash input candidates org-db-v3--fulltext-cache)
                candidates)
            ;; Error or no results
            (list (format "Search failed or no results for: %s" input)))))))

(defun org-db-v3--build-ivy-fulltext-candidates (results)
  "Build ivy candidates from fulltext RESULTS array.
Each candidate is a string with metadata stored as text properties."
  (let ((candidates nil))
    (dotimes (i (length results))
      (let* ((result (aref results i))
             (filename (alist-get 'filename result))
             (title (alist-get 'title result))
             (snippet (alist-get 'snippet result))
             (rank (alist-get 'rank result))
             ;; Extract search term from snippet (text between >>> and <<<)
             (search-term (when (string-match ">>>\\([^<]+\\)<<<" snippet)
                           (match-string 1 snippet)))
             ;; Clean snippet for display
             (clean-snippet (replace-regexp-in-string
                            "[\n\r]+" " "
                            (replace-regexp-in-string ">>>\\|<<<" "" snippet)))
             (snippet-width 60)
             (display-snippet (if (> (length clean-snippet) snippet-width)
                                 (concat (substring clean-snippet 0 (- snippet-width 3)) "...")
                               clean-snippet))
             (padded-snippet (format (format "%%-%ds" snippet-width) display-snippet))
             ;; Format: rank | snippet | filename
             (candidate (format "%8.2f | %s | %s"
                               (abs rank)
                               padded-snippet
                               (file-name-nondirectory filename))))

        ;; Store metadata as text properties
        (put-text-property 0 (length candidate) 'fulltext-data
                          `(:file ,filename :title ,title :search-term ,search-term)
                          candidate)
        (push candidate candidates)))
    (nreverse candidates)))

(defun org-db-v3--open-fulltext-candidate (candidate)
  "Open file for selected fulltext CANDIDATE.
CANDIDATE is a string with metadata stored in text properties."
  (let* ((data (get-text-property 0 'fulltext-data candidate))
         (file (plist-get data :file))
         (search-term (plist-get data :search-term))
         (title (plist-get data :title)))
    (when (and file (file-exists-p file))
      (find-file file)
      (goto-char (point-min))
      ;; Try to search for the matched term first, then title
      (if (and search-term (search-forward search-term nil t))
          (progn
            (beginning-of-line)
            (recenter))
        (when (search-forward title nil t)
          (beginning-of-line)
          (recenter))))))

;; Configure ivy for fulltext search if available
(with-eval-after-load 'ivy
  (ivy-configure 'org-db-v3-fulltext-search-ivy
    :height 15
    :sort-fn nil))  ; Keep relevance order from API

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

;;;###autoload
(defun org-db-v3-open-linked-file ()
  "Browse all linked files (PDF, DOCX, etc.) in database and open selected file or org link."
  (interactive)

  (org-db-v3-ensure-server)

  (plz 'get (concat (org-db-v3-server-url) "/api/linked-files/all")
    :as #'json-read
    :then (lambda (response)
            (org-db-v3-display-linked-file-list response))
    :else (lambda (error)
            (message "Error fetching linked files: %s" (plz-error-message error)))))

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

(defun org-db-v3-display-linked-file-list (response)
  "Display linked file list from RESPONSE using completing-read."
  (let* ((linked-files (alist-get 'linked_files response))
         (count (alist-get 'count response)))

    (if (zerop count)
        (message "No linked files found in database")

      ;; Build candidates with metadata
      (let* ((candidates nil)
             (metadata-table (make-hash-table :test 'equal)))

        (dotimes (i (length linked-files))
          (let* ((file-info (aref linked-files i))
                 (file-path (alist-get 'file_path file-info))
                 (file-type (alist-get 'file_type file-info))
                 (org-filename (alist-get 'org_filename file-info))
                 (org-link-line (alist-get 'org_link_line file-info))
                 (chunk-count (alist-get 'chunk_count file-info))
                 (indexed-at (alist-get 'indexed_at file-info))
                 ;; Format timestamp for display (remove microseconds if present)
                 (display-time (if indexed-at
                                  (replace-regexp-in-string "\\..*" "" indexed-at)
                                "unknown"))
                 ;; Format: [TYPE] filename | org-file:line | chunks | timestamp
                 (candidate (format "[%-4s] %-40s | %s:%d | %d chunks | %s"
                                   (upcase (or file-type ""))
                                   (file-name-nondirectory file-path)
                                   (file-name-nondirectory org-filename)
                                   org-link-line
                                   chunk-count
                                   display-time)))

            ;; Store metadata
            (puthash candidate
                    (list :file-path file-path
                          :org-filename org-filename
                          :org-link-line org-link-line)
                    metadata-table)
            (push candidate candidates)))

        ;; Keep chronological order (most recent first, already sorted from server)
        (setq candidates (nreverse candidates))

        ;; Let user select
        (let ((selection (completing-read
                         (format "Open linked file (%d files in database): " count)
                         candidates
                         nil t)))
          (when selection
            (let* ((metadata (gethash selection metadata-table))
                   (file-path (plist-get metadata :file-path))
                   (org-filename (plist-get metadata :org-filename))
                   (org-link-line (plist-get metadata :org-link-line)))
              ;; With prefix arg, open the linked file directly
              ;; Otherwise, open the org file and go to the link
              (if current-prefix-arg
                  (if (and file-path (file-exists-p file-path))
                      (find-file file-path)
                    (message "Linked file does not exist: %s" file-path))
                ;; Default: open org file at link location
                (when (and org-filename (file-exists-p org-filename))
                  (find-file org-filename)
                  (goto-char (point-min))
                  (forward-line (1- org-link-line))
                  (recenter))))))))))

(provide 'org-db-v3-search)
;;; org-db-v3-search.el ends here
