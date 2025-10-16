;;; test_ivy_request.el --- Test ivy-style dynamic request -*- lexical-binding: t; -*-

;; This simulates what org-db-v3--dynamic-semantic-collection does

(require 'json)
(require 'url)

(defun test-dynamic-search (query)
  "Test dynamic semantic search with QUERY."
  (let* ((url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (request-body `((query . ,query)
                        (limit . 20)
                        (rerank . :json-false)
                        (rerank_candidates . 50)))
         (url-request-data (encode-coding-string
                            (json-encode request-body)
                            'utf-8))
         (url "http://127.0.0.1:8765/api/search/semantic"))

    (message "Sending request to: %s" url)
    (message "Request body: %s" url-request-data)

    (condition-case err
        (let ((buffer (url-retrieve-synchronously url t nil 5)))
          (if (not buffer)
              (progn
                (message "ERROR: No buffer returned from url-retrieve-synchronously")
                nil)
            (message "Got buffer: %s" (buffer-name buffer))
            (unwind-protect
                (with-current-buffer buffer
                  (message "Buffer contents (first 500 chars):")
                  (message "%s" (buffer-substring-no-properties
                                (point-min)
                                (min (+ (point-min) 500) (point-max))))
                  (goto-char (point-min))
                  (if (re-search-forward "^$" nil t)
                      (progn
                        (message "Found end of headers at position: %d" (point))
                        (let ((response (json-read)))
                          (message "Parsed JSON successfully")
                          (message "Response: %S" response)
                          response))
                    (message "ERROR: Could not find end of HTTP headers")
                    nil))
              (kill-buffer buffer))))
      (error
       (message "ERROR during request: %S" err)
       nil))))

;; Test with a short query like ivy would send
(message "\n=== Testing dynamic search ===")
(test-dynamic-search "mushroom")
