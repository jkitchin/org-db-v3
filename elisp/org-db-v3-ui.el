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

;;;###autoload (autoload 'org-db-menu "org-db-v3-ui" nil t)
(transient-define-prefix org-db-menu ()
  "org-db v3 - Search and manage your org files."
  ["Search"
   ["Text Search"
    ("s" "Semantic search" org-db-v3-semantic-search
     :description "Search by meaning using embeddings")
    ("f" "Full-text search" org-db-v3-fulltext-search
     :description "Search by keywords (FTS5)")
    ("a" "Search at point" org-db-v3-search-at-point
     :description "Search using text at point/region")]
   ["Image Search"
    ("i" "Search images" org-db-v3-image-search
     :description "Find images by text description")]]
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
     :description "Open server homepage in browser")]]
  ["Options"
   ("q" "Quit" transient-quit-one)])

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
  "View the server log buffer."
  (interactive)
  (let ((buffer (get-buffer "*org-db-server*")))
    (if buffer
        (pop-to-buffer buffer)
      (message "Server not started via org-db-v3-start-server. Check your terminal/shell for logs."))))

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
