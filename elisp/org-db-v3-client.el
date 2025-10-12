;;; org-db-v3-client.el --- HTTP client for org-db -*- lexical-binding: t; -*-

;;; Commentary:
;; Async HTTP client using plz.el to communicate with the server.

;;; Code:

(require 'plz)
(require 'json)
;; (require 'org-db-v3)
(require 'org-db-v3-parse)
(require 'org-db-v3-server)

(defun org-db-v3-index-file-async (filename)
  "Index FILENAME asynchronously by sending to server."
  (org-db-v3-ensure-server)

  (when (file-exists-p filename)
    (with-current-buffer (find-file-noselect filename)
      (let ((json-data (org-db-v3-parse-buffer-to-json)))
        (plz 'post (concat (org-db-v3-server-url) "/api/file")
          :headers '(("Content-Type" . "application/json"))
          :body json-data
          :as #'json-read
          :then (lambda (response)
                  (message "Indexed %s (%d headlines)"
                           filename
                           (alist-get 'headlines_count response)))
          :else (lambda (error)
                  (message "Error indexing %s: %s" filename error)))))))

;;;###autoload
(defun org-db-v3-index-directory (directory)
  "Recursively index all org files in DIRECTORY."
  (interactive "DDirectory to index: ")
  (let* ((org-files (directory-files-recursively
                     directory
                     "\\.org\\(\\.gpg\\)?\\'"
                     nil
                     (lambda (dir)
                       ;; Skip hidden directories and common ignore patterns
                       (not (string-match-p "/\\." (file-name-nondirectory dir))))))
         (count (length org-files)))
    (if (zerop count)
        (message "No org files found in %s" directory)
      (when (yes-or-no-p (format "Index %d org file%s in %s? "
                                 count
                                 (if (= count 1) "" "s")
                                 directory))
        (message "Indexing %d org file%s..." count (if (= count 1) "" "s"))
        (dolist (file org-files)
          (org-db-v3-index-file-async file))
        (message "Sent %d file%s to server for indexing"
                 count
                 (if (= count 1) "" "s"))))))

;;;###autoload
(defun org-db-v3-reindex-database ()
  "Reindex all files currently in the database.
Fetches the list of files from the server and reindexes each one.
Also removes files that no longer exist from the database."
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
                    ;; Reindex existing files
                    (when existing-files
                      (message "Reindexing %d file%s..." (length existing-files)
                              (if (= (length existing-files) 1) "" "s"))
                      (dolist (filename existing-files)
                        (org-db-v3-index-file-async filename)))

                    ;; Delete missing files from database
                    (when missing-files
                      (message "Removing %d missing file%s from database..."
                              (length missing-files)
                              (if (= (length missing-files) 1) "" "s"))
                      (dolist (filename missing-files)
                        (org-db-v3-delete-file-async filename)))

                    ;; Final summary
                    (message "Reindex complete: %d file%s reindexed%s"
                            (length existing-files)
                            (if (= (length existing-files) 1) "" "s")
                            (if missing-files
                                (format ", %d file%s removed"
                                       (length missing-files)
                                       (if (= (length missing-files) 1) "" "s"))
                              "")))))))
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

(provide 'org-db-v3-client)
;;; org-db-v3-client.el ends here
