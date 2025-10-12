;;; org-db-v3-ui.el --- Transient menu interface -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: org-db-v3
;; Keywords: org-mode, database, search

;;; Commentary:

;; Transient menu interface for org-db v3 commands.
;; Provides an easy-to-use menu system for all search and management functions.

;;; Code:

(require 'transient)
(require 'org-db-v3-search)
(require 'org-db-v3-server)
(require 'org-db-v3-client)
(require 'org-db-v3-agenda)

(defvar org-db-v3-search-scope '(all . nil)
  "Current search scope. Format: (type . value)
   - (all . nil) - search all files
   - (directory . \"/path/to/dir/\") - files under directory
   - (project . \"/path/to/project/\") - files in project root
   - (tag . \"tag-name\") - files with specific keyword/tag
   Resets to (all . nil) after each search.")

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

;;;###autoload
(defun org-db-v3-scope-all ()
  "Set search scope to all files."
  (interactive)
  (setq org-db-v3-search-scope '(all . nil))
  (message "Scope: All files (next search only)"))

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

;;;###autoload (autoload 'org-db-menu "org-db-v3-ui" nil t)
(transient-define-prefix org-db-menu ()
  [:description (lambda () (format "org-db v3 [Scope: %s]" (org-db-v3--scope-description)))
   "Search and manage your org files."]
  ["Options"
   ("-s" "Set scope..." org-db-v3-scope-menu)
   ("q" "Quit" transient-quit-one)]
  ["Search"
   ["Text Search"
    ("v" "Semantic search" org-db-v3-semantic-search
     :description "Search by meaning using embeddings")
    ("k" "Full-text search" org-db-v3-fulltext-search
     :description "Search by keywords (FTS5)")
    ("h" "Headline search" org-db-v3-headline-search
     :description "Search/browse headlines and jump to them")
    ("p" "Search at point" org-db-v3-search-at-point
     :description "Search using text at point/region")]
   ["Image Search"
    ("i" "Search images" org-db-v3-image-search
     :description "Find images by text description")]
   ["Files"
    ("f" "Open file from db" org-db-v3-open-file
     :description "Browse and open files in database")]]
  ["Agenda"
   ("a" "Show agenda" org-db-v3-agenda
    :description "Show TODO items with deadlines and scheduled dates")]
  ["Management"
   ["Indexing"
    ("u" "Update current file" org-db-v3-update-current-file
     :description "Re-index the current org file")
    ("U" "Update all open files" org-db-v3-update-all-buffers
     :description "Re-index all open org buffers")
    ("d" "Index directory" org-db-v3-index-directory
     :description "Recursively index all org files in a directory")
    ("r" "Reindex database" org-db-v3-reindex-database
     :description "Reindex all files currently in the database")]
   ["Server"
    ("S" "Server status" org-db-v3-server-status
     :description "Check if server is running")
    ("R" "Restart server" org-db-v3-restart-server
     :description "Stop and start the server")
    ("L" "View server logs" org-db-v3-view-logs
     :description "Open server log buffer")
    ("W" "Open web interface" org-db-v3-open-web-interface
     :description "Open server homepage in browser")]])

;;;###autoload
(defun org-db-v3-update-current-file ()
  "Manually update the current file."
  (interactive)
  (if (buffer-file-name)
      (progn
        (org-db-v3-index-file-async (buffer-file-name))
        (message "Indexing %s..." (buffer-file-name)))
    (message "No file associated with current buffer")))

;;;###autoload
(defun org-db-v3-update-all-buffers ()
  "Update all open org buffers."
  (interactive)
  (let ((count 0))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (buffer-file-name)
                   (or (string-suffix-p ".org" (buffer-file-name))
                       (string-suffix-p ".org_archive" (buffer-file-name))))
          (org-db-v3-index-file-async (buffer-file-name))
          (setq count (1+ count)))))
    (message "Sent %d org file%s to server for indexing"
             count
             (if (= count 1) "" "s"))))

;;;###autoload
(defun org-db-v3-server-status ()
  "Show server status."
  (interactive)
  (org-db-v3-ensure-server)
  (if (org-db-v3-server-running-p)
      (message "org-db server is running on %s" (org-db-v3-server-url))
    (message "org-db server is not running")))

;;;###autoload
(defun org-db-v3-restart-server ()
  "Restart the org-db server."
  (interactive)
  (when (yes-or-no-p "Restart org-db server? ")
    (org-db-v3-stop-server)
    (sleep-for 1)
    (org-db-v3-start-server)
    (message "Server restarted")))

;;;###autoload
(defun org-db-v3-view-logs ()
  "View the server log file."
  (interactive)
  (let ((log-file "/tmp/org-db-server.log"))
    (if (file-exists-p log-file)
        (progn
          (find-file-other-window log-file)
          (goto-char (point-max))
          (auto-revert-tail-mode 1)
          (message "Viewing server logs from %s" log-file))
      (message "Server log file not found: %s" log-file))))

;;;###autoload
(defun org-db-v3-open-web-interface ()
  "Open the org-db server web interface in a browser."
  (interactive)
  (org-db-v3-ensure-server)
  (if (org-db-v3-server-running-p)
      (progn
        (browse-url (org-db-v3-server-url))
        (message "Opening org-db web interface at %s" (org-db-v3-server-url)))
    (message "org-db server is not running")))

;; Make org-db-menu available as M-x org-db
;;;###autoload
(defalias 'org-db 'org-db-menu)

(provide 'org-db-v3-ui)
;;; org-db-v3-ui.el ends here
