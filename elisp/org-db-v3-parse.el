;;; org-db-v3-parse.el --- Org parsing to JSON -*- lexical-binding: t; -*-

;;; Commentary:
;; Functions to parse org-mode files and convert to JSON for the server.

;;; Code:

(require 'org)
(require 'org-element)
(require 'json)

(defun org-db-v3-parse-headlines (parse-tree)
  "Extract headlines from PARSE-TREE as a list of plists."
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
        (list :title (org-element-property :raw-value hl)
              :level (org-element-property :level hl)
              :todo-keyword (org-element-property :todo-keyword hl)
              :todo-type (symbol-name (or (org-element-property :todo-type hl) 'nil))
              :archivedp (org-element-property :archivedp hl)
              :commentedp (org-element-property :commentedp hl)
              :begin begin
              :end end
              :tags (when tags
                     (concat ":" (mapconcat #'identity tags ":") ":"))
              :priority (when-let* ((p (org-element-property :priority hl)))
                         (char-to-string p))
              :scheduled (when scheduled
                          (org-timestamp-format scheduled "%Y-%m-%d %H:%M:%S"))
              :deadline (when deadline
                         (org-timestamp-format deadline "%Y-%m-%d %H:%M:%S"))
              :properties properties)))))

(defun org-db-v3-parse-links (parse-tree)
  "Extract links from PARSE-TREE as a list of plists."
  (org-element-map parse-tree 'link
    (lambda (link)
      (list :type (org-element-property :type link)
            :path (org-element-property :path link)
            :raw-link (org-element-property :raw-link link)
            :description (when (org-element-property :contents-begin link)
                          (buffer-substring-no-properties
                           (org-element-property :contents-begin link)
                           (org-element-property :contents-end link)))
            :search-option (org-element-property :search-option link)
            :begin (org-element-property :begin link)))))

(defun org-db-v3-parse-keywords (parse-tree)
  "Extract keywords from PARSE-TREE as a list of plists."
  (org-element-map parse-tree 'keyword
    (lambda (kw)
      (list :key (upcase (org-element-property :key kw))
            :value (org-element-property :value kw)
            :begin (org-element-property :begin kw)))))

(defun org-db-v3-parse-src-blocks (parse-tree)
  "Extract src blocks from PARSE-TREE as a list of plists."
  (org-element-map parse-tree 'src-block
    (lambda (src)
      (list :language (org-element-property :language src)
            :contents (org-element-property :value src)
            :begin (org-element-property :begin src)))))

(defun org-db-v3-parse-images (parse-tree)
  "Extract image links from PARSE-TREE as a list of plists."
  (org-element-map parse-tree 'link
    (lambda (link)
      (let ((type (org-element-property :type link))
            (path (org-element-property :path link)))
        ;; Filter for image file types
        (when (and (member type '("file" "attachment"))
                   (string-match-p "\\.\\(png\\|jpg\\|jpeg\\|gif\\|svg\\|webp\\)\\'" path))
          (list :path path
                :begin (org-element-property :begin link)))))))

(defun org-db-v3-parse-buffer-to-json ()
  "Parse current org buffer and return JSON string for server."
  (let* ((parse-tree (org-element-parse-buffer))
         (file-size (when (buffer-file-name)
                     (nth 7 (file-attributes (buffer-file-name)))))
         (data (list :filename (buffer-file-name)
                     :md5 (md5 (current-buffer))
                     :file-size (or file-size 0)
                     :content (buffer-string)
                     :headlines (org-db-v3-parse-headlines parse-tree)
                     :links (org-db-v3-parse-links parse-tree)
                     :keywords (org-db-v3-parse-keywords parse-tree)
                     :src-blocks (org-db-v3-parse-src-blocks parse-tree)
                     :images (org-db-v3-parse-images parse-tree))))
    (json-encode data)))

(provide 'org-db-v3-parse)
;;; org-db-v3-parse.el ends here
