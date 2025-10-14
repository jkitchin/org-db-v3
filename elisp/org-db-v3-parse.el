;;; org-db-v3-parse.el --- Org parsing to JSON -*- lexical-binding: t; -*-

;;; Commentary:
;; Functions to parse org-mode files and convert to JSON for the server.

;;; Code:

(require 'org)
(require 'org-element)
(require 'json)
(require 'seq)  ; For seq-difference

(defun org-db-v3-parse-headlines (parse-tree)
  "Extract headlines from PARSE-TREE as a list of alists."
  (org-element-map parse-tree 'headline
    (lambda (hl)
      (let* ((begin (org-element-property :begin hl))
             (end (org-element-property :end hl))
             (tags (org-element-property :tags hl))
             (scheduled (org-element-property :scheduled hl))
             (deadline (org-element-property :deadline hl))
             (properties (save-excursion
                          (goto-char begin)
                          (org-entry-properties))))
        `(("title" . ,(org-element-property :raw-value hl))
          ("level" . ,(org-element-property :level hl))
          ("todo_keyword" . ,(org-element-property :todo-keyword hl))
          ("todo_type" . ,(symbol-name (or (org-element-property :todo-type hl) 'nil)))
          ("archivedp" . ,(org-element-property :archivedp hl))
          ("commentedp" . ,(org-element-property :commentedp hl))
          ("begin" . ,begin)
          ("end" . ,end)
          ("tags" . ,(when tags
                       (concat ":" (mapconcat #'identity tags ":") ":")))
          ("priority" . ,(when-let* ((p (org-element-property :priority hl)))
                           (char-to-string p)))
          ("scheduled" . ,(when scheduled
                            (org-timestamp-format scheduled "%Y-%m-%d %H:%M:%S")))
          ("deadline" . ,(when deadline
                           (org-timestamp-format deadline "%Y-%m-%d %H:%M:%S")))
          ("properties" . ,properties))))))

(defun org-db-v3-parse-links (parse-tree)
  "Extract links from PARSE-TREE as a list of alists."
  (org-element-map parse-tree 'link
    (lambda (link)
      `(("type" . ,(org-element-property :type link))
        ("path" . ,(org-element-property :path link))
        ("raw_link" . ,(org-element-property :raw-link link))
        ("description" . ,(when (org-element-property :contents-begin link)
                            (buffer-substring-no-properties
                             (org-element-property :contents-begin link)
                             (org-element-property :contents-end link))))
        ("search_option" . ,(org-element-property :search-option link))
        ("begin" . ,(org-element-property :begin link))))))

(defun org-db-v3-parse-keywords (parse-tree)
  "Extract keywords from PARSE-TREE as a list of alists."
  (org-element-map parse-tree 'keyword
    (lambda (kw)
      `(("key" . ,(upcase (org-element-property :key kw)))
        ("value" . ,(org-element-property :value kw))
        ("begin" . ,(org-element-property :begin kw))))))

(defun org-db-v3-parse-src-blocks (parse-tree)
  "Extract src blocks from PARSE-TREE as a list of alists."
  (org-element-map parse-tree 'src-block
    (lambda (src)
      `(("language" . ,(org-element-property :language src))
        ("contents" . ,(org-element-property :value src))
        ("begin" . ,(org-element-property :begin src))))))

(defun org-db-v3-parse-images (parse-tree)
  "Extract image links from PARSE-TREE as a list of alists."
  (org-element-map parse-tree 'link
    (lambda (link)
      (let ((type (org-element-property :type link))
            (path (org-element-property :path link)))
        ;; Filter for image file types
        (when (and (member type '("file" "attachment"))
                   (string-match-p "\\.\\(png\\|jpg\\|jpeg\\|gif\\|svg\\|webp\\)\\'" path))
          `(("path" . ,path)
            ("begin" . ,(org-element-property :begin link))))))))

(defcustom org-db-v3-index-linked-files t
  "Whether to index files linked from org files using docling.
When enabled, files like PDF, DOCX, XLSX, etc. will be converted
to markdown and indexed for semantic search."
  :type 'boolean
  :group 'org-db-v3)

(defcustom org-db-v3-linked-file-extensions
  '("pdf" "docx" "doc" "xlsx" "xls" "pptx" "ppt"
    "html" "htm" "xhtml" "md" "markdown" "asciidoc" "adoc"
    "csv" "png" "jpg" "jpeg" "tiff" "tif" "bmp" "webp"
    "vtt" "xml" "json")
  "List of file extensions that should be indexed via docling.
These are files that docling can convert to markdown for indexing."
  :type '(repeat string)
  :group 'org-db-v3)

(defcustom org-db-v3-max-linked-file-size (* 50 1024 1024)
  "Maximum file size in bytes for linked files to index.
Default is 50MB. Files larger than this will be skipped."
  :type 'integer
  :group 'org-db-v3)

(defcustom org-db-v3-index-images t
  "Whether to index image files linked from org files.
Images will be processed with OCR when indexed."
  :type 'boolean
  :group 'org-db-v3)

(defcustom org-db-v3-index-audio-files nil
  "Whether to index audio files linked from org files.
Audio files are processed with ASR (speech recognition), which is
slow and resource-intensive. Disabled by default."
  :type 'boolean
  :group 'org-db-v3)

(defun org-db-v3-parse-linked-files (parse-tree)
  "Extract file links from PARSE-TREE that should be indexed via docling.
Returns a list of alists with file path and line number."
  (when org-db-v3-index-linked-files
    (let* ((image-exts '("png" "jpg" "jpeg" "tiff" "tif" "bmp" "webp"))
           (audio-exts '("wav" "mp3" "m4a" "ogg"))
           (doc-exts (seq-difference org-db-v3-linked-file-extensions
                                     (append image-exts audio-exts))))
      (org-element-map parse-tree 'link
        (lambda (link)
          (let ((type (org-element-property :type link))
                (path (org-element-property :path link))
                (begin (org-element-property :begin link)))
            ;; Handle file links - org-mode may use "file", "attachment", or "fuzzy" for local files
            (when (and path (member type '("file" "attachment" "fuzzy")))
              ;; Expand path if relative
              (let* ((expanded-path (expand-file-name path
                                                      (file-name-directory (buffer-file-name))))
                     (ext (downcase (or (file-name-extension expanded-path) ""))))
                ;; Only process if: has extension, file exists, and matches our criteria
                (when (and (not (string-empty-p ext))
                          (file-exists-p expanded-path)
                          (or (member ext doc-exts)
                              (and org-db-v3-index-images (member ext image-exts))
                              (and org-db-v3-index-audio-files (member ext audio-exts))))
                  `(("file_path" . ,expanded-path)
                    ("org_link_line" . ,(line-number-at-pos begin))))))))))))

(defun org-db-v3-count-linked-files ()
  "Count number of indexable linked files in current buffer."
  (interactive)
  (let* ((parse-tree (org-element-parse-buffer))
         (linked-files (org-db-v3-parse-linked-files parse-tree)))
    (message "Found %d indexable linked files" (length linked-files))
    (length linked-files)))

(defun org-db-v3-debug-linked-files ()
  "Debug function to show why linked files are or aren't found."
  (interactive)
  (let* ((parse-tree (org-element-parse-buffer))
         (all-links (org-element-map parse-tree 'link
                      (lambda (link)
                        (list (org-element-property :type link)
                              (org-element-property :path link)))))
         (image-exts '("png" "jpg" "jpeg" "tiff" "tif" "bmp" "webp"))
         (audio-exts '("wav" "mp3" "m4a" "ogg"))
         (doc-exts (seq-difference org-db-v3-linked-file-extensions
                                   (append image-exts audio-exts))))
    (with-output-to-temp-buffer "*org-db-v3-debug*"
      (princ (format "org-db-v3-index-linked-files: %s\n\n" org-db-v3-index-linked-files))
      (princ (format "Document extensions (doc-exts): %s\n\n" doc-exts))
      (princ (format "All links found in buffer (%d):\n" (length all-links)))
      (dolist (link all-links)
        (let* ((type (car link))
               (path (cadr link))
               (expanded-path (when path
                               (expand-file-name path
                                                (file-name-directory (buffer-file-name)))))
               (ext (when expanded-path
                     (downcase (or (file-name-extension expanded-path) ""))))
               (exists (when expanded-path (file-exists-p expanded-path)))
               (should-index (and ext
                                 (not (string-empty-p ext))
                                 (or (member ext doc-exts)
                                     (and org-db-v3-index-images (member ext image-exts))
                                     (and org-db-v3-index-audio-files (member ext audio-exts))))))
          (princ (format "\n  Link: [[%s:%s]]\n" type path))
          (princ (format "    Expanded: %s\n" expanded-path))
          (princ (format "    Extension: %s\n" ext))
          (princ (format "    File exists: %s\n" exists))
          (princ (format "    Should index: %s\n" should-index))
          (when (and should-index exists)
            (princ "    ✓ WILL BE INDEXED\n"))))
      (princ (format "\n\nLinked files that will be sent to server: %d\n"
                    (length (org-db-v3-parse-linked-files parse-tree)))))))

(defun org-db-v3-parse-buffer-to-json ()
  "Parse current org buffer and return JSON string for server."
  (let* ((parse-tree (org-element-parse-buffer))
         (file-size (when (buffer-file-name)
                     (nth 7 (file-attributes (buffer-file-name)))))
         (data `(("filename" . ,(buffer-file-name))
                 ("md5" . ,(md5 (current-buffer)))
                 ("file_size" . ,(or file-size 0))
                 ("content" . ,(buffer-substring-no-properties (point-min) (point-max)))
                 ("headlines" . ,(vconcat (org-db-v3-parse-headlines parse-tree)))
                 ("links" . ,(vconcat (org-db-v3-parse-links parse-tree)))
                 ("keywords" . ,(vconcat (org-db-v3-parse-keywords parse-tree)))
                 ("src_blocks" . ,(vconcat (org-db-v3-parse-src-blocks parse-tree)))
                 ("images" . ,(vconcat (org-db-v3-parse-images parse-tree)))
                 ("linked_files" . ,(vconcat (org-db-v3-parse-linked-files parse-tree))))))
    (json-encode data)))

(provide 'org-db-v3-parse)
;;; org-db-v3-parse.el ends here
