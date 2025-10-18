;;; org-db-v3-server.el --- Server management -*- lexical-binding: t; -*-

;;; Commentary:
;; Functions to start, stop, and check the org-db server.

;;; Code:

(require 'ansi-color)

;; (require 'org-db-v3)

(defcustom org-db-v3-python-command "uv"
  "Command to run Python (uv, python3, etc)."
  :type 'string
  :group 'org-db-v3)

(defcustom org-db-v3-server-directory nil
  "Directory containing the Python server code.
If nil, automatically detected relative to the elisp directory."
  :type '(choice (const :tag "Auto-detect" nil)
                 (directory :tag "Custom directory"))
  :group 'org-db-v3)

(defun org-db-v3--get-server-directory ()
  "Get the server directory, auto-detecting if not set."
  (or org-db-v3-server-directory
      (let* ((elisp-dir (file-name-directory (locate-library "org-db-v3-server")))
             (project-root (file-name-directory (directory-file-name elisp-dir))))
        (expand-file-name "python" project-root))))

(defun org-db-v3--port-in-use-p (port)
  "Check if PORT is already in use by another process."
  (let ((result (shell-command-to-string
                 (format "lsof -i :%d -sTCP:LISTEN 2>/dev/null || netstat -an 2>/dev/null | grep ':%d.*LISTEN'"
                         port port))))
    (not (string-empty-p (string-trim result)))))

(defun org-db-v3-kill-zombie-processes ()
  "Kill any zombie org-db server processes on the configured port.
Returns t if any processes were killed, nil otherwise.
IMPORTANT: Only kills processes that are NOT managed by Emacs (not org-db-v3-server-process)."
  (interactive)
  (let* ((port org-db-v3-server-port)
         (lsof-output (shell-command-to-string
                       (format "lsof -i :%d -sTCP:LISTEN 2>/dev/null | tail -n +2 | awk '{print $2}'"
                               port)))
         (pids (split-string (string-trim lsof-output) "\n" t))
         ;; Get the PID of the Emacs-managed server process
         (emacs-server-pid (when (and org-db-v3-server-process
                                      (process-live-p org-db-v3-server-process))
                             (number-to-string (process-id org-db-v3-server-process))))
         (zombie-pids (if emacs-server-pid
                          (seq-remove (lambda (pid) (string= pid emacs-server-pid)) pids)
                        pids)))
    (when zombie-pids
      (message "Found %d zombie process(es) on port %d (excluding Emacs-managed server), attempting to kill..."
               (length zombie-pids) port)
      (dolist (pid zombie-pids)
        (message "Killing zombie process %s..." pid)
        (shell-command (format "kill -9 %s 2>/dev/null" pid)))
      (sleep-for 0.5)
      (message "Zombie processes cleaned up")
      t)
    (when (and pids (not zombie-pids))
      (message "Port %d is in use by Emacs-managed server (PID %s), not killing it"
               port emacs-server-pid)
      nil)))

(defun org-db-v3-start-server ()
  "Start the org-db server.
Includes protection against concurrent starts and port conflicts."
  (interactive)
  (cond
   ;; Already running
   ((org-db-v3-server-running-p)
    (message "org-db server already running on %s:%d"
             org-db-v3-server-host org-db-v3-server-port))

   ;; Currently starting (prevent race condition)
   (org-db-v3-server-starting
    (message "org-db server is already starting, please wait..."))

   ;; Port already in use by another process
   ((org-db-v3--port-in-use-p org-db-v3-server-port)
    (if (yes-or-no-p (format "Port %d already in use. Kill zombie processes and restart? "
                             org-db-v3-server-port))
        (progn
          (org-db-v3-kill-zombie-processes)
          (org-db-v3-start-server))
      (message "Port %d already in use. Server may already be running outside Emacs."
               org-db-v3-server-port)))

   ;; Safe to start
   (t
    (setq org-db-v3-server-starting t)
    (unwind-protect
        (let* ((default-directory (org-db-v3--get-server-directory))
               (process-name "org-db-server")
               (buffer-name "*org-db-server*")
               (cmd (list org-db-v3-python-command "run" "uvicorn"
                         "org_db_server.main:app" "--reload"
                         "--host" org-db-v3-server-host
                         "--port" (number-to-string org-db-v3-server-port))))

          (setq org-db-v3-server-process
                (make-process
                 :name process-name
                 :buffer buffer-name
                 :command cmd
                 :filter #'org-db-v3-server-filter
                 :sentinel #'org-db-v3-server-sentinel))

          ;; Wait for server to start with retry logic (with feedback)
          (message "Starting org-db server on %s:%d..."
                   org-db-v3-server-host org-db-v3-server-port)

          ;; Poll for server readiness with exponential backoff
          ;; Total max wait: 1 + 2 + 3 + 4 + 5 = 15 seconds
          (let ((retries 5)
                (retry-delays '(1 2 3 4 5))
                (started nil))
            (while (and retry-delays (not started))
              (sleep-for (car retry-delays))
              (setq retry-delays (cdr retry-delays))
              (when (org-db-v3-server-running-p)
                (setq started t)))

            (if started
                (message "org-db server started on %s:%d"
                         org-db-v3-server-host org-db-v3-server-port)
              ;; Check if port conflict after failed start
              (with-current-buffer buffer-name
                (goto-char (point-min))
                (if (re-search-forward "Address already in use" nil t)
                    (message "Failed to start: port %d already in use" org-db-v3-server-port)
                  (pop-to-buffer buffer-name)
                  (goto-char (point-max))
                  (error "Failed to start org-db server. See buffer for details"))))))
      ;; Always clear the starting flag
      (setq org-db-v3-server-starting nil)))))

(defun org-db-v3-stop-server ()
  "Stop the org-db server."
  (interactive)
  (when (and org-db-v3-server-process
             (process-live-p org-db-v3-server-process))
    ;; First try graceful shutdown via API
    (condition-case err
        (let ((url-request-method "POST")
              (url-request-extra-headers '(("Content-Type" . "application/json"))))
          (with-timeout (2 nil)
            (url-retrieve-synchronously
             (concat (org-db-v3-server-url) "/api/shutdown")
             t nil 2)))
      (error
       (when org-db-v3-debug
         (message "org-db-v3: Graceful shutdown failed: %S" err))))

    ;; Give it a moment to shut down gracefully
    (sleep-for 0.5)

    ;; If still running, force kill
    (when (process-live-p org-db-v3-server-process)
      (kill-process org-db-v3-server-process))

    (setq org-db-v3-server-process nil
          org-db-v3-server-starting nil)
    (message "org-db server stopped")))

(defun org-db-v3-server-filter (process output)
  "Filter PROCESS OUTPUT to colorize ANSI escape codes."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (let ((moving (= (point) (process-mark process))))
        (save-excursion
          ;; Insert the text, advancing the process marker
          (goto-char (process-mark process))
          (let ((start (point)))
            (insert output)
            (ansi-color-apply-on-region start (point)))
          (set-marker (process-mark process) (point)))
        ;; If point was at end, keep it at end
        (when moving
          (goto-char (process-mark process)))))))

(defun org-db-v3-server-sentinel (process event)
  "Sentinel for server PROCESS EVENT."
  (cond
   ;; Normal termination (e.g., user called org-db-v3-stop-server)
   ((string-match-p "finished" event)
    (message "org-db server stopped normally")
    (setq org-db-v3-server-process nil
          org-db-v3-server-starting nil))

   ;; Abnormal exit during startup
   ((and (string-match-p "\\(exited\\|failed\\)" event)
         org-db-v3-server-starting)
    (message "org-db server failed to start: %s" (string-trim event))
    (setq org-db-v3-server-process nil
          org-db-v3-server-starting nil)
    (with-current-buffer (process-buffer process)
      (pop-to-buffer (current-buffer))
      (goto-char (point-max))))

   ;; Abnormal exit after successful startup (crash)
   ((string-match-p "\\(exited\\|failed\\)" event)
    (message "org-db server crashed: %s" (string-trim event))
    (setq org-db-v3-server-process nil
          org-db-v3-server-starting nil))))

(defun org-db-v3-ensure-server ()
  "Ensure server is running, start if needed.
Automatically cleans up zombie processes without prompting when auto-starting."
  (unless (org-db-v3-server-running-p)
    (when org-db-v3-auto-start-server
      ;; Check if we have a tracked server process
      (if (and org-db-v3-server-process
               (process-live-p org-db-v3-server-process))
          ;; We have a live process, but health check failed
          ;; This is likely because the server is busy, not dead
          (when org-db-v3-debug
            (message "org-db-v3: Server process alive but health check failed (likely busy)"))
        ;; No tracked process, or it's dead
        (when (org-db-v3--port-in-use-p org-db-v3-server-port)
          ;; Port in use but no tracked process = zombie
          (message "Cleaning up zombie processes on port %d..." org-db-v3-server-port)
          (org-db-v3-kill-zombie-processes))
        (org-db-v3-start-server)))))

(defun org-db-v3-colorize-server-buffer ()
  "Colorize ANSI escape codes in the *org-db-server* buffer.
Useful for colorizing output that was already in the buffer before
the filter was added."
  (interactive)
  (when-let ((buffer (get-buffer "*org-db-server*")))
    (with-current-buffer buffer
      (ansi-color-apply-on-region (point-min) (point-max))
      (message "Colorized *org-db-server* buffer"))))

(defun org-db-v3-kill-emacs-hook ()
  "Hook function to cleanly shut down the org-db server when Emacs exits.
This ensures the database connection is closed properly."
  (when (and org-db-v3-server-process
             (process-live-p org-db-v3-server-process))
    (message "Shutting down org-db server...")
    (org-db-v3-stop-server)))

;; Register the kill-emacs-hook to ensure clean shutdown
(add-hook 'kill-emacs-hook #'org-db-v3-kill-emacs-hook)

(provide 'org-db-v3-server)
;;; org-db-v3-server.el ends here
