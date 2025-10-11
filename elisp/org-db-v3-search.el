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

(defcustom org-db-v3-search-results-buffer "*org-db search*"
  "Buffer name for displaying search results."
  :type 'string
  :group 'org-db-v3)

(defcustom org-db-v3-search-default-limit 10
  "Default number of search results to retrieve."
  :type 'integer
  :group 'org-db-v3)

(defvar org-db-v3-last-search-results nil
  "Store last search results for navigation.")

;;;###autoload
(defun org-db-v3-semantic-search (query &optional limit)
  "Perform semantic search for QUERY.
Retrieve up to LIMIT results (default `org-db-v3-search-default-limit')."
  (interactive (list (read-string "Search query: ")
                    (when current-prefix-arg
                      (read-number "Limit: " org-db-v3-search-default-limit))))

  (org-db-v3-ensure-server)

  (let ((limit (or limit org-db-v3-search-default-limit)))
    (plz 'post (concat (org-db-v3-server-url) "/api/search/semantic")
      :headers '(("Content-Type" . "application/json"))
      :body (json-encode `((query . ,query)
                          (limit . ,limit)))
      :as #'json-read
      :then (lambda (response)
              (org-db-v3-display-search-results query response))
      :else (lambda (error)
              (message "Search error: %s" (plz-error-message error))))))

(defun org-db-v3-display-search-results (query response)
  "Display search RESPONSE for QUERY in results buffer."
  (let ((results (alist-get 'results response))
        (model-used (alist-get 'model_used response)))

    ;; Store results for navigation
    (setq org-db-v3-last-search-results results)

    (with-current-buffer (get-buffer-create org-db-v3-search-results-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)

        ;; Insert header
        (insert (format "* Semantic Search Results: \"%s\"\n\n" query))
        (insert (format "Model: %s | Results: %d\n\n" model-used (length results)))

        (if (zerop (length results))
            (insert "No results found.\n")

          ;; Insert results
          (dotimes (i (length results))
            (let* ((result (aref results i))
                   (chunk-text (alist-get 'chunk_text result))
                   (filename (alist-get 'filename result))
                   (similarity (alist-get 'similarity_score result))
                   (begin-line (alist-get 'begin_line result))
                   (end-line (alist-get 'end_line result)))

              (insert (format "** Result %d (score: %.3f)\n" (1+ i) similarity))
              (insert (format "   :PROPERTIES:\n"))
              (insert (format "   :FILE: %s\n" filename))
              (insert (format "   :LINE: %d-%d\n" begin-line end-line))
              (insert (format "   :END:\n\n"))
              (insert (format "   %s\n\n" chunk-text)))))

        ;; Set up buffer
        (goto-char (point-min))
        (setq buffer-read-only t)
        (org-db-v3-search-mode))

      ;; Display buffer
      (pop-to-buffer (current-buffer)))))

(defvar org-db-v3-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'org-db-v3-search-goto-result)
    (define-key map (kbd "n") #'org-db-v3-search-next-result)
    (define-key map (kbd "p") #'org-db-v3-search-previous-result)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "s") #'org-db-v3-semantic-search)
    map)
  "Keymap for `org-db-v3-search-mode'.")

(define-derived-mode org-db-v3-search-mode org-mode "org-db-search"
  "Major mode for org-db search results.

Key bindings:
\\{org-db-v3-search-mode-map}"
  (setq buffer-read-only t))

(defun org-db-v3-search-goto-result ()
  "Go to the file and location of the search result at point."
  (interactive)
  (let* ((file (org-entry-get (point) "FILE"))
         (line-range (org-entry-get (point) "LINE"))
         (line (when line-range
                 (string-to-number (car (split-string line-range "-"))))))

    (if (and file (file-exists-p file))
        (progn
          (find-file-other-window file)
          (when line
            (goto-char (point-min))
            (forward-line (1- line)))
          (recenter))
      (message "File not found: %s" (or file "unknown")))))

(defun org-db-v3-search-next-result ()
  "Move to next search result."
  (interactive)
  (org-next-visible-heading 1))

(defun org-db-v3-search-previous-result ()
  "Move to previous search result."
  (interactive)
  (org-previous-visible-heading 1))

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

  (let ((limit (or limit org-db-v3-search-default-limit)))
    (plz 'post (concat (org-db-v3-server-url) "/api/search/fulltext")
      :headers '(("Content-Type" . "application/json"))
      :body (json-encode `((query . ,query)
                          (limit . ,limit)))
      :as #'json-read
      :then (lambda (response)
              (org-db-v3-display-fulltext-results query response))
      :else (lambda (error)
              (message "Search error: %s" (plz-error-message error))))))

(defun org-db-v3-display-fulltext-results (query response)
  "Display full-text search RESPONSE for QUERY in results buffer."
  (let ((results (alist-get 'results response)))

    (with-current-buffer (get-buffer-create org-db-v3-search-results-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)

        ;; Insert header
        (insert (format "* Full-Text Search Results: \"%s\"\n\n" query))
        (insert (format "Results: %d\n\n" (length results)))

        (if (zerop (length results))
            (insert "No results found.\n")

          ;; Insert results
          (dotimes (i (length results))
            (let* ((result (aref results i))
                   (filename (alist-get 'filename result))
                   (title (alist-get 'title result))
                   (content (alist-get 'content result))
                   (tags (alist-get 'tags result)))

              (insert (format "** Result %d: %s\n" (1+ i) title))
              (insert (format "   :PROPERTIES:\n"))
              (insert (format "   :FILE: %s\n" filename))
              (insert (format "   :TAGS: %s\n" (or tags "")))
              (insert (format "   :END:\n\n"))
              (insert (format "   %s\n\n" content)))))

        ;; Set up buffer
        (goto-char (point-min))
        (setq buffer-read-only t)
        (org-db-v3-search-mode))

      ;; Display buffer
      (pop-to-buffer (current-buffer)))))

(provide 'org-db-v3-search)
;;; org-db-v3-search.el ends here
