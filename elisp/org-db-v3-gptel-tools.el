;;; org-db-v3-gptel-tools.el --- gptel tools for org-db search -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides gptel tools that allow LLMs to search your org files using
;; semantic and fulltext search capabilities of org-db-v3.

;;; Code:

(require 'gptel)
(require 'org-db-v3-search)
(require 'json)

(defcustom org-db-v3-gptel-search-limit 5
  "Default number of search results to return to LLM tools."
  :type 'integer
  :group 'org-db-v3)

(defun org-db-v3-gptel--format-search-result (result)
  "Format a single search RESULT for LLM consumption."
  (let ((filename (alist-get 'filename result))
        (chunk-text (alist-get 'chunk_text result))
        (similarity (alist-get 'similarity_score result))
        (begin-line (alist-get 'begin_line result))
        (linked-file (alist-get 'linked_file_path result))
        (linked-type (alist-get 'linked_file_type result)))
    (format "File: %s%s\nLine: %d\nRelevance: %.3f\nContent:\n%s\n"
            filename
            (if linked-file
                (format " [linked %s file: %s]"
                       (upcase (or linked-type ""))
                       (file-name-nondirectory linked-file))
              "")
            begin-line
            similarity
            chunk-text)))

(defun org-db-v3-gptel--semantic-search (query &optional limit filename-pattern)
  "Perform semantic search for QUERY.
Returns up to LIMIT results (default from `org-db-v3-gptel-search-limit').
Optional FILENAME-PATTERN restricts search to matching files."
  (let* ((search-limit (or limit org-db-v3-gptel-search-limit))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (request-data `((query . ,query)
                        (limit . ,search-limit)
                        ,@(when filename-pattern
                            `((filename_pattern . ,filename-pattern)))))
         (url-request-data (encode-coding-string
                           (json-encode request-data)
                           'utf-8))
         (response-buffer (url-retrieve-synchronously
                          (concat org-db-v3-server-url "/api/search/semantic")
                          t nil 10)))
    (if (not response-buffer)
        (error "Failed to connect to org-db server")
      (with-current-buffer response-buffer
        (goto-char (point-min))
        (re-search-forward "^$")
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (json-key-type 'symbol)
               (response (json-read))
               (results (alist-get 'results response)))
          (kill-buffer)
          (if (null results)
              "No results found."
            (mapconcat #'org-db-v3-gptel--format-search-result
                      results
                      "\n---\n")))))))

(defun org-db-v3-gptel--fulltext-search (query &optional limit filename-pattern)
  "Perform fulltext search for QUERY.
Returns up to LIMIT results (default from `org-db-v3-gptel-search-limit').
Optional FILENAME-PATTERN restricts search to matching files."
  (let* ((search-limit (or limit org-db-v3-gptel-search-limit))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (request-data `((query . ,query)
                        (limit . ,search-limit)
                        ,@(when filename-pattern
                            `((filename_pattern . ,filename-pattern)))))
         (url-request-data (encode-coding-string
                           (json-encode request-data)
                           'utf-8))
         (response-buffer (url-retrieve-synchronously
                          (concat org-db-v3-server-url "/api/search/fulltext")
                          t nil 10)))
    (if (not response-buffer)
        (error "Failed to connect to org-db server")
      (with-current-buffer response-buffer
        (goto-char (point-min))
        (re-search-forward "^$")
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (json-key-type 'symbol)
               (response (json-read))
               (results (alist-get 'results response)))
          (kill-buffer)
          (if (null results)
              "No results found."
            (mapconcat
             (lambda (result)
               (let ((filename (alist-get 'filename result))
                     (title (alist-get 'title result))
                     (snippet (alist-get 'snippet result))
                     (rank (alist-get 'rank result)))
                 (format "File: %s\nTitle: %s\nRank: %.3f\nSnippet:\n%s\n"
                        filename
                        (or title "(no title)")
                        rank
                        snippet)))
             results
             "\n---\n")))))))

;;;###autoload
(defun org-db-v3-gptel-register-tools ()
  "Register org-db-v3 search tools with gptel."
  (interactive)
  (gptel-make-tool
   :function #'org-db-v3-gptel--semantic-search
   :name "org_semantic_search"
   :description "Search through org-mode files and linked documents (PDF, DOCX, PPTX, etc.) using semantic/AI similarity. Best for conceptual queries, finding related content, or when you don't know exact keywords. Returns relevant passages with context."
   :args (list
          '(:name "query"
            :type "string"
            :description "Natural language query describing what to search for. Can be a question, concept, or topic.")
          '(:name "limit"
            :type "number"
            :description "Maximum number of results to return (default 5, max 20)"
            :optional t)
          '(:name "filename_pattern"
            :type "string"
            :description "SQL LIKE pattern to filter files (e.g., '%project%' for files containing 'project'). Use '%' as wildcard."
            :optional t))
   :category "org-db")

  (gptel-make-tool
   :function #'org-db-v3-gptel--fulltext-search
   :name "org_fulltext_search"
   :description "Search through org-mode files using exact keyword matching (FTS5). Best for finding specific terms, names, or exact phrases. Faster than semantic search but requires knowing the right keywords. Returns snippets with matched terms highlighted."
   :args (list
          '(:name "query"
            :type "string"
            :description "Search terms or phrase to find. Supports AND/OR/NOT operators and quoted phrases.")
          '(:name "limit"
            :type "number"
            :description "Maximum number of results to return (default 5, max 20)"
            :optional t)
          '(:name "filename_pattern"
            :type "string"
            :description "SQL LIKE pattern to filter files (e.g., '%2024%' for files in 2024). Use '%' as wildcard."
            :optional t))
   :category "org-db")

  (message "org-db-v3 search tools registered with gptel"))

;;;###autoload
(defun org-db-v3-gptel-unregister-tools ()
  "Unregister org-db-v3 search tools from gptel."
  (interactive)
  ;; Remove tools by filtering gptel-tools
  (setq gptel-tools
        (seq-remove (lambda (tool)
                     (member (plist-get tool :name)
                            '("org_semantic_search" "org_fulltext_search")))
                   gptel-tools))
  (message "org-db-v3 search tools unregistered from gptel"))

(provide 'org-db-v3-gptel-tools)
;;; org-db-v3-gptel-tools.el ends here
