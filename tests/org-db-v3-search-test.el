;;; org-db-v3-search-test.el --- Tests for org-db-v3-search -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for semantic search functionality.
;; These tests focus on display and navigation, not network calls.

;;; Code:

(require 'ert)
(require 'org)

;; Load search module directly
(add-to-list 'load-path (expand-file-name "elisp" default-directory))
(load-file "elisp/org-db-v3-search.el")

(ert-deftest org-db-v3-search-mode-exists ()
  "Test that org-db-v3-search-mode is defined."
  (should (fboundp 'org-db-v3-search-mode)))

(ert-deftest org-db-v3-search-commands-exist ()
  "Test that search commands are defined."
  (should (fboundp 'org-db-v3-semantic-search))
  (should (fboundp 'org-db-v3-search-at-point))
  (should (fboundp 'org-db-v3-search-goto-result)))

(ert-deftest org-db-v3-search-buffer-creation ()
  "Test that search results buffer can be created."
  (with-temp-buffer
    (org-db-v3-search-mode)
    (should (eq major-mode 'org-db-v3-search-mode))
    (should buffer-read-only)))

(ert-deftest org-db-v3-display-search-results ()
  "Test displaying search results."
  (let ((query "test query")
        (response '((results . [((chunk_text . "Test content")
                                (filename . "/tmp/test.org")
                                (similarity_score . 0.95)
                                (begin_line . 10)
                                (end_line . 15))])
                   (model_used . "all-MiniLM-L6-v2"))))

    (org-db-v3-display-search-results query response)

    (with-current-buffer org-db-v3-search-results-buffer
      (should (eq major-mode 'org-db-v3-search-mode))
      (let ((content (buffer-substring-no-properties (point-min) (point-max))))
        ;; Check that key elements are present
        (should (string-match-p "test query" content))
        (should (string-match-p "all-MiniLM-L6-v2" content))
        (should (string-match-p "Test content" content))
        ;; Check for score (may be formatted different ways)
        (should (string-match-p "0\\.95" content))))))

(ert-deftest org-db-v3-display-empty-results ()
  "Test displaying empty search results."
  (let ((query "no results")
        (response '((results . [])
                   (model_used . "all-MiniLM-L6-v2"))))

    (org-db-v3-display-search-results query response)

    (with-current-buffer org-db-v3-search-results-buffer
      (goto-char (point-min))
      (should (search-forward "No results found" nil t)))))

(provide 'org-db-v3-search-test)
;;; org-db-v3-search-test.el ends here
