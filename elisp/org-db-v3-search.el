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

(defcustom org-db-v3-search-default-limit 10
  "Default number of search results to retrieve."
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
  "Display search RESPONSE for QUERY using completing-read."
  (let* ((results (alist-get 'results response))
         (model-used (alist-get 'model_used response)))

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
                 (display-text (replace-regexp-in-string
                               "[\n\r]+" " "
                               (if (> (length chunk-text) 80)
                                   (concat (substring chunk-text 0 77) "...")
                                 chunk-text)))
                 ;; Format: "context... | filename:line"
                 (candidate (format "%.3f | %s | %s:%d"
                                   similarity
                                   display-text
                                   (file-name-nondirectory filename)
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
                         (format "Search results (%s, %d found): "
                                model-used (length results))
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
                 ;; Truncate content for display
                 (display-text (replace-regexp-in-string
                               "[\n\r]+" " "
                               (if (> (length content) 60)
                                   (concat (substring content 0 57) "...")
                                 content)))
                 ;; Format: "title | context... | filename"
                 (candidate (format "%s | %s | %s"
                                   title
                                   display-text
                                   (file-name-nondirectory filename))))

            ;; Store metadata
            (puthash candidate
                    (list :file filename
                          :title title
                          :content content
                          :tags tags)
                    metadata-table)
            (push candidate candidates)))

        ;; Reverse to show in order
        (setq candidates (nreverse candidates))

        ;; Let user select
        (let ((selection (completing-read
                         (format "Fulltext results (%d found): " (length results))
                         candidates
                         nil t)))
          (when selection
            (let* ((metadata (gethash selection metadata-table))
                   (file (plist-get metadata :file)))
              (when (and file (file-exists-p file))
                (find-file file)
                ;; Try to search for the title in the file
                (goto-char (point-min))
                (when (search-forward (plist-get metadata :title) nil t)
                  (beginning-of-line)
                  (recenter))))))))))

;;;###autoload
(defun org-db-v3-image-search (query &optional limit)
  "Perform image search for QUERY using CLIP embeddings.
Retrieve up to LIMIT results (default `org-db-v3-search-default-limit')."
  (interactive (list (read-string "Image search query: ")
                    (when current-prefix-arg
                      (read-number "Limit: " org-db-v3-search-default-limit))))

  (org-db-v3-ensure-server)

  (let ((limit (or limit org-db-v3-search-default-limit)))
    (plz 'post (concat (org-db-v3-server-url) "/api/search/images")
      :headers '(("Content-Type" . "application/json"))
      :body (json-encode `((query . ,query)
                          (limit . ,limit)))
      :as #'json-read
      :then (lambda (response)
              (org-db-v3-display-image-results query response))
      :else (lambda (error)
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
                 ;; Format: "score | image-filename | org-filename"
                 (candidate (format "%.3f | %s | %s"
                                    similarity
                                    (file-name-nondirectory image-path)
                                    (file-name-nondirectory filename))))

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

(provide 'org-db-v3-search)
;;; org-db-v3-search.el ends here
