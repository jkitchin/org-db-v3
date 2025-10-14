;;; org-db-v3-client.el --- HTTP client for org-db -*- lexical-binding: t; -*-

;;; Commentary:
;; Async HTTP client using plz.el to communicate with the server.

;;; Code:

(require 'plz)
(require 'json)
;; (require 'org-db-v3)
(require 'org-db-v3-parse)
(require 'org-db-v3-server)

;; Queue for non-blocking directory indexing
(defvar org-db-v3-index-queue nil
  "Queue of files waiting to be indexed.")

(defvar org-db-v3-index-timer nil
  "Timer for processing the index queue.")

(defvar org-db-v3-index-total 0
  "Total number of files in current indexing operation.")

(defvar org-db-v3-index-processed 0
  "Number of files processed in current indexing operation.")

(defcustom org-db-v3-index-delay 0.5
  "Delay in seconds between indexing files.
Lower values = faster indexing but less responsive Emacs and higher server load.
Higher values = slower indexing but more responsive Emacs and prevents server overload.
Default 0.5s allows time for linked file processing to complete."
  :type 'number
  :group 'org-db-v3)

(defun org-db-v3-index-file-async (filename)
  "Index FILENAME asynchronously by sending to server.
Disables local variables and hooks for safe and fast bulk indexing.
Skips Emacs temporary files (.#*, #*#, *~)."
  (org-db-v3-ensure-server)

  (let ((basename (file-name-nondirectory filename)))
    ;; Skip Emacs temporary files
    (when (or (string-prefix-p ".#" basename)
              (and (string-prefix-p "#" basename)
                   (string-suffix-p "#" basename))
              (string-suffix-p "~" basename))
      (error "Skipping Emacs temporary file: %s" filename)))

  (when (file-exists-p filename)
    (let ((already-open (find-buffer-visiting filename))
          buf)
      ;; Bind these BEFORE opening the file so they take effect during file loading
      (let ((enable-local-variables nil)  ; Don't evaluate local variables
            (enable-dir-local-variables nil)  ; Don't evaluate directory-local variables
            (org-mode-hook '()))  ; Skip org-mode hooks
        (setq buf (or already-open (find-file-noselect filename))))

      (with-current-buffer buf
        (let ((json-data (org-db-v3-parse-buffer-to-json)))
          (plz 'post (concat (org-db-v3-server-url) "/api/file")
            :headers '(("Content-Type" . "application/json"))
            :body json-data
            :as #'json-read
            :timeout 120  ; Allow time for docling PDF/DOCX conversion
            :then (lambda (response)
                    (let ((headlines (alist-get 'headlines_count response))
                          (linked-files (alist-get 'linked_files_count response 0)))
                      (if (> linked-files 0)
                          (message "Indexed %s (%d headlines, %d linked file%s)"
                                   filename headlines linked-files
                                   (if (= linked-files 1) "" "s"))
                        (message "Indexed %s (%d headlines)"
                                 filename headlines))))
            :else (lambda (error)
                    (message "Error indexing %s: %s" filename error))))
        ;; Kill buffer if it wasn't already open
        (unless already-open
          (kill-buffer buf))))))

(defun org-db-v3-process-index-queue ()
  "Process one file from the index queue.
Waits for each request to complete before processing the next file."
  (when org-db-v3-index-queue
    (let ((filename (pop org-db-v3-index-queue)))
      (setq org-db-v3-index-processed (1+ org-db-v3-index-processed))

      ;; Update progress in echo area
      (message "Indexing [%d/%d]: %s"
               org-db-v3-index-processed
               org-db-v3-index-total
               (file-name-nondirectory filename))

      ;; Index the file and continue queue on completion
      (org-db-v3-index-file-with-continuation filename)))

  ;; If queue is empty, clean up
  (when (null org-db-v3-index-queue)
    ;; Clear the indexing marker
    (setq org-db-v3-index-timer nil)
    (message "Indexing complete: %d file%s processed"
             org-db-v3-index-total
             (if (= org-db-v3-index-total 1) "" "s"))))

(defun org-db-v3-index-file-with-continuation (filename)
  "Index FILENAME and continue processing queue on completion.
This ensures requests are processed sequentially, not in parallel."
  (org-db-v3-ensure-server)

  (let ((basename (file-name-nondirectory filename)))
    ;; Skip Emacs temporary files
    (when (or (string-prefix-p ".#" basename)
              (and (string-prefix-p "#" basename)
                   (string-suffix-p "#" basename))
              (string-suffix-p "~" basename))
      (error "Skipping Emacs temporary file: %s" filename)))

  (when (file-exists-p filename)
    (let ((already-open (find-buffer-visiting filename))
          buf)
      ;; Bind these BEFORE opening the file so they take effect during file loading
      (let ((enable-local-variables nil)  ; Don't evaluate local variables
            (enable-dir-local-variables nil)  ; Don't evaluate directory-local variables
            (org-mode-hook '()))  ; Skip org-mode hooks
        (setq buf (or already-open (find-file-noselect filename))))

      (with-current-buffer buf
        (let ((json-data (org-db-v3-parse-buffer-to-json)))
          (plz 'post (concat (org-db-v3-server-url) "/api/file")
            :headers '(("Content-Type" . "application/json"))
            :body json-data
            :as #'json-read
            :timeout 120  ; Allow time for linked file conversion
            :then (lambda (response)
                    (let ((headlines (alist-get 'headlines_count response))
                          (linked-files (alist-get 'linked_files_count response 0)))
                      (if (> linked-files 0)
                          (message "Indexed %s (%d headlines, %d linked file%s)"
                                   filename headlines linked-files
                                   (if (= linked-files 1) "" "s"))
                        (message "Indexed %s (%d headlines)"
                                 filename headlines)))
                    ;; Process next file in queue after this one completes
                    (run-with-timer org-db-v3-index-delay nil #'org-db-v3-process-index-queue))
            :else (lambda (error)
                    (message "Error indexing %s: %s" filename error)
                    ;; Continue with next file even on error
                    (run-with-timer org-db-v3-index-delay nil #'org-db-v3-process-index-queue))))
        ;; Kill buffer if it wasn't already open
        (unless already-open
          (kill-buffer buf))))))

;;;###autoload
(defun org-db-v3-index-directory (directory)
  "Recursively index all org files in DIRECTORY.
Files are processed one at a time using timers to keep Emacs responsive."
  (interactive "DDirectory to index: ")
  (let* ((all-files (directory-files-recursively
                     directory
                     "\\.org\\(\\.gpg\\)?\\'"
                     nil
                     (lambda (dir)
                       ;; Skip hidden directories and common ignore patterns
                       (not (string-match-p "/\\." (file-name-nondirectory dir))))))
         ;; Filter out Emacs temporary files
         (org-files (seq-filter
                     (lambda (file)
                       (let ((basename (file-name-nondirectory file)))
                         (not (or
                               ;; Emacs lock files: .#filename.org
                               (string-prefix-p ".#" basename)
                               ;; Emacs auto-save files: #filename.org#
                               (and (string-prefix-p "#" basename)
                                    (string-suffix-p "#" basename))
                               ;; Emacs backup files: filename.org~
                               (string-suffix-p "~" basename)))))
                     all-files))
         (count (length org-files)))
    (if (zerop count)
        (message "No org files found in %s" directory)
      (when (yes-or-no-p (format "Index %d org file%s in %s? "
                                 count
                                 (if (= count 1) "" "s")
                                 directory))
        ;; Check if indexing is already in progress
        (when org-db-v3-index-timer
          (message "Indexing already in progress, please wait or cancel first")
          (user-error "Indexing already in progress"))

        ;; Set up the queue
        (setq org-db-v3-index-queue org-files
              org-db-v3-index-total count
              org-db-v3-index-processed 0)

        ;; Start processing first file (continuation handled in callback)
        (setq org-db-v3-index-timer t)  ; Marker that indexing is active
        (run-with-timer 0 nil #'org-db-v3-process-index-queue)

        (message "Starting sequential indexing of %d file%s..."
                 count
                 (if (= count 1) "" "s"))))))

;;;###autoload
(defun org-db-v3-reindex-database ()
  "Reindex all files currently in the database.
Fetches the list of files from the server and reindexes each one.
Also removes files that no longer exist from the database.
Uses non-blocking queue processing to keep Emacs responsive."
  (interactive)
  (org-db-v3-ensure-server)

  (plz 'get (concat (org-db-v3-server-url) "/api/files")
    :as #'json-read
    :then (lambda (response)
            (let* ((files (alist-get 'files response))
                   (count (length files))
                   (missing-files nil)
                   (existing-files nil))

              ;; Classify files as existing or missing
              (dotimes (i count)
                (let ((filename (alist-get 'filename (aref files i))))
                  (if (file-exists-p filename)
                      (push filename existing-files)
                    (push filename missing-files))))

              (if (zerop count)
                  (message "No files found in database")

                ;; Show summary and confirm
                (let ((msg (format "Reindex %d existing file%s%s? "
                                  (length existing-files)
                                  (if (= (length existing-files) 1) "" "s")
                                  (if missing-files
                                      (format " (and remove %d missing file%s)"
                                             (length missing-files)
                                             (if (= (length missing-files) 1) "" "s"))
                                    ""))))
                  (when (yes-or-no-p msg)
                    ;; Delete missing files from database immediately
                    (when missing-files
                      (message "Removing %d missing file%s from database..."
                              (length missing-files)
                              (if (= (length missing-files) 1) "" "s"))
                      (dolist (filename missing-files)
                        (org-db-v3-delete-file-async filename)))

                    ;; Reindex existing files using non-blocking queue
                    (when existing-files
                      ;; Check if indexing is already in progress
                      (when org-db-v3-index-timer
                        (message "Indexing already in progress, please wait or cancel first")
                        (user-error "Indexing already in progress"))

                      ;; Set up the queue
                      (setq org-db-v3-index-queue existing-files
                            org-db-v3-index-total (length existing-files)
                            org-db-v3-index-processed 0)

                      ;; Start processing first file (continuation handled in callback)
                      (setq org-db-v3-index-timer t)  ; Marker that indexing is active
                      (run-with-timer 0 nil #'org-db-v3-process-index-queue)

                      (message "Starting sequential reindex of %d file%s..."
                               (length existing-files)
                               (if (= (length existing-files) 1) "" "s"))))))))
    :else (lambda (error)
            (message "Error fetching file list: %s" (plz-error-message error)))))

(defun org-db-v3-delete-file-async (filename)
  "Delete FILENAME from the database asynchronously."
  (plz 'delete (concat (org-db-v3-server-url) "/api/file?filename=" (url-hexify-string filename))
    :as #'json-read
    :then (lambda (response)
            (message "Removed %s from database" filename))
    :else (lambda (error)
            (message "Error removing %s: %s" filename (plz-error-message error)))))

;;;###autoload
(defun org-db-v3-cancel-indexing ()
  "Cancel the current indexing operation."
  (interactive)
  (if org-db-v3-index-timer
      (progn
        (setq org-db-v3-index-timer nil)
        (message "Indexing cancelled: %d of %d files processed"
                 org-db-v3-index-processed
                 org-db-v3-index-total)
        (setq org-db-v3-index-queue nil
              org-db-v3-index-total 0
              org-db-v3-index-processed 0))
    (message "No indexing operation in progress")))

(provide 'org-db-v3-client)
;;; org-db-v3-client.el ends here
