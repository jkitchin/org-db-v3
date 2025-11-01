;;; org-db-v3-agenda.el --- Agenda view for org-db v3 -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: org-db-v3
;; Keywords: org-mode, database, agenda

;;; Commentary:

;; Agenda functionality for org-db v3.
;; Shows TODO items with deadlines and scheduled dates.

;;; Code:

(require 'json)
(require 'org)

;; Forward declarations
(declare-function org-db-v3-server-url "org-db-v3")
(declare-function org-db-v3-ensure-server "org-db-v3")
(declare-function ivy-read "ivy")

;; Require plz only when available
(require 'plz nil t)

(unless (fboundp 'org-db-v3-server-url)
  (defun org-db-v3-server-url ()
    "Return the base URL for the org-db server."
    (format "http://%s:%d"
            (if (boundp 'org-db-v3-server-host) org-db-v3-server-host "127.0.0.1")
            (if (boundp 'org-db-v3-server-port) org-db-v3-server-port 8765))))

(unless (fboundp 'org-db-v3-ensure-server)
  (defun org-db-v3-ensure-server ()
    "Ensure server is running (stub for testing)."
    nil))

;; Global variable to store metadata table for ivy actions
(defvar org-db-v3--agenda-metadata-table nil
  "Hash table storing metadata for agenda candidates.")

(defun org-db-v3--agenda-open-file (candidate)
  "Open file for agenda CANDIDATE."
  (when-let* ((metadata (gethash candidate org-db-v3--agenda-metadata-table))
              (file (plist-get metadata :file))
              (begin (plist-get metadata :begin)))
    (when (file-exists-p file)
      (find-file file)
      (goto-char begin)
      (org-show-entry)
      (recenter))))

(defun org-db-v3--agenda-delete-file (candidate)
  "Delete file from database for agenda CANDIDATE."
  (when-let* ((metadata (gethash candidate org-db-v3--agenda-metadata-table))
              (file (plist-get metadata :file)))
    (when (yes-or-no-p (format "Delete %s from database? " file))
      (org-db-v3-ensure-server)
      (plz 'delete (concat (org-db-v3-server-url) "/api/file?filename=" (url-hexify-string file))
        :as #'json-read
        :then (lambda (response)
                (message "Deleted: %s" (alist-get 'message response)))
        :else (lambda (error)
                (message "Delete error: %s" (plz-error-message error)))))))

;;;###autoload
(defun org-db-v3-agenda (&optional before-date)
  "Show agenda of TODO items with deadlines/scheduled dates.
By default shows items due in the next 2 weeks.
With prefix argument, prompts for a custom date range.
BEFORE-DATE is a date string like \"+2w\", \"+1m\", or \"2025-12-31\"."
  (interactive (list (when current-prefix-arg
                       (read-string "Show items before (e.g. +2w, +1m): " "+2w"))))
  (let ((before (or before-date "+2w")))
    (org-db-v3-ensure-server)

    (plz 'post (concat (org-db-v3-server-url) "/api/agenda")
      :headers '(("Content-Type" . "application/json"))
      :body (json-encode `((before . ,before)))
      :as #'json-read
      :then (lambda (response)
              (org-db-v3-display-agenda-results before response))
      :else (lambda (error)
              (message "Agenda error: %s" (plz-error-message error))))))

(defun org-db-v3-display-agenda-results (before response)
  "Display agenda RESPONSE for items before BEFORE date."
  (let ((results (alist-get 'results response)))

    (if (zerop (length results))
        (message "No TODO items found before %s" before)

      ;; Build candidates with metadata
      (let ((candidates nil))
        ;; Use global metadata table for ivy actions
        (setq org-db-v3--agenda-metadata-table (make-hash-table :test 'equal))

        (dotimes (i (length results))
          (let* ((result (elt results i))
                 (title (alist-get 'title result))
                 (filename (alist-get 'filename result))
                 (begin (alist-get 'begin result))
                 (level (alist-get 'level result))
                 (todo-keyword (alist-get 'todo_keyword result))
                 (priority (alist-get 'priority result))
                 (deadline (alist-get 'deadline result))
                 (scheduled (alist-get 'scheduled result))
                 (tags (alist-get 'tags result))
                 ;; Format the date field (deadline or scheduled)
                 (date-str (or deadline scheduled ""))
                 (date-display (if (> (length date-str) 28)
                                  (substring date-str 0 28)
                                date-str))
                 ;; Build the title with priority and stars
                 (title-prefix (concat (make-string level ?*)
                                      " "
                                      (if priority (format "[#%s] " priority) "")
                                      todo-keyword
                                      " "))
                 (full-title (concat title-prefix title))
                 ;; Fixed widths for alignment
                 (date-width 28)
                 (title-width 80)
                 (tags-width 20)
                 (padded-date (format (format "%%-%ds" date-width) date-display))
                 (padded-title (if (> (length full-title) title-width)
                                  (concat (substring full-title 0 (- title-width 3)) "...")
                                full-title))
                 (padded-title-final (format (format "%%-%ds" title-width) padded-title))
                 (padded-tags (format (format "%%-%ds" tags-width) (or tags "")))
                 ;; Format: date | title | tags | filename
                 (candidate (format "%s | %s | %s | %s"
                                   padded-date
                                   padded-title-final
                                   padded-tags
                                   filename))
                 ;; Determine face based on date
                 (face (cond
                        ;; gcal.org items get a pleasing blue color
                        ((string-match-p "gcal\\.org" filename)
                         '(:foreground "steel blue"))
                        ((string= priority "A") '(:foreground "red1" :weight bold))
                        (scheduled '(:foreground "DarkOrange3"))
                        ((and deadline
                              ;; Compare dates using string comparison (YYYY-MM-DD format)
                              (string< (substring deadline 0 10)
                                      (format-time-string "%Y-%m-%d")))
                         '(:foreground "dark red"))
                        (t '(:foreground "green4"))))
                 (candidate-with-face (propertize candidate 'face face)))

            ;; Store metadata
            (puthash candidate-with-face
                    (list :file filename
                          :begin begin
                          :title title
                          :level level
                          :deadline deadline
                          :scheduled scheduled
                          :todo-keyword todo-keyword
                          :priority priority
                          :tags tags)
                    org-db-v3--agenda-metadata-table)
            (push candidate-with-face candidates)))

        ;; Reverse to show in chronological order
        (setq candidates (nreverse candidates))

        ;; Let user select with ivy actions
        (if (fboundp 'ivy-read)
            (ivy-read (format "Agenda (%d items before %s): " (length results) before)
                      candidates
                      :caller 'org-db-v3-agenda
                      :action '(1
                               ("o" org-db-v3--agenda-open-file "Open file")
                               ("d" org-db-v3--agenda-delete-file "Delete file from db")))
          ;; Fallback to completing-read if ivy not available
          (let ((selection (completing-read
                           (format "Agenda (%d items before %s): " (length results) before)
                           candidates
                           nil t)))
            (when selection
              (org-db-v3--agenda-open-file selection))))))))

(provide 'org-db-v3-agenda)
;;; org-db-v3-agenda.el ends here
