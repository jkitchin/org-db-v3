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

;; Load additional modules
(require 'org-db-v3-parse)
(require 'org-db-v3-client)
(require 'org-db-v3-server)
(require 'org-db-v3-search)
(require 'org-db-v3-ui)

(defun org-db-v3-hook-function ()
  "Hook function for org-mode files."
  (when (and (buffer-file-name)
             (or (string-suffix-p ".org" (buffer-file-name))
                 (string-suffix-p ".org_archive" (buffer-file-name))))
    (add-hook 'after-save-hook #'org-db-v3-after-save-hook nil t)))

(defun org-db-v3-after-save-hook ()
  "Hook to run after saving an org file."
  (when (buffer-file-name)
    (org-db-v3-index-file-async (buffer-file-name))))

(defun org-db-v3-enable ()
  "Enable org-db v3."
  (interactive)
  (add-hook 'org-mode-hook #'org-db-v3-hook-function)
  (message "org-db v3 enabled"))

(defun org-db-v3-disable ()
  "Disable org-db v3."
  (interactive)
  (remove-hook 'org-mode-hook #'org-db-v3-hook-function)
  (message "org-db v3 disabled"))

(provide 'org-db-v3)
;;; org-db-v3.el ends here
