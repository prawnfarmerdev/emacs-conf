;;; my-ssh.el --- SSH sessionizer with consult and perspective integration -*- lexical-binding: t -*-

;;; Commentary:
;; SSH sessionizer functionality that:
;; 1. Reads server inventory from a CSV file (hostname,username)
;; 2. Provides interactive server selection using Consult
;; 3. Creates a perspective named after the selected server
;; 4. Opens eshell with SSH connection (ssh username@hostname)
;; 5. Optionally detects if SSH key needs copying (ssh-copy-id)

;;==============================================================================
;; CONFIGURATION VARIABLES
;;==============================================================================

(defvar my/ssh-server-csv-file (expand-file-name "~/.emacs.d/data/servers.csv")
  "Path to CSV file containing server inventory.
CSV format: hostname,username (one per line, header optional)")

(defvar my/ssh-default-username (user-login-name)
  "Default username to use if not specified in CSV.")

(defvar my/ssh-prompt-for-key-copy t
  "If non-nil, automatically copy SSH key when needed (no prompt).")

(defvar my/ssh-csv-hostname-column 0
  "Column index (0-based) for hostname in CSV file.")

(defvar my/ssh-csv-username-column 1
  "Column index (0-based) for username in CSV file.
If column doesn't exist, use default username.")

(defvar my/ssh-use-tramp t
  "If non-nil, use TRAMP for remote connections instead of eshell SSH.
TRAMP provides better integration with Emacs buffers and files.")

(defvar my/ssh-tramp-mode 'dired-and-terminal
  "TRAMP connection mode: 'dired, 'shell, or 'dired-and-terminal.
When `my/ssh-use-tramp' is non-nil, determines what to open:
- 'dired: Open dired on remote home directory
- 'shell: Open shell buffer connected via TRAMP
- 'dired-and-terminal: Open dired then a terminal with same TRAMP path")

;;==============================================================================
;; CSV PARSING
;;==============================================================================

(defun my/ssh-parse-csv-file (csv-file)
  "Parse CSV-FILE and return alist of (hostname . username).
Skips header line if present (first line containing 'hostname').
Each line should have format: hostname,username"
  (when (file-exists-p csv-file)
    (with-temp-buffer
      (insert-file-contents csv-file)
      (goto-char (point-min))
      (let (servers)
        ;; Skip header line if it contains 'hostname'
        (when (looking-at-p ".*hostname.*")
          (forward-line 1))
        ;; Process each line
        (while (not (eobp))
          (let ((line (string-trim (thing-at-point 'line t))))
            (unless (or (string-empty-p line)
                        (string-match-p "^#" line))  ; Skip comments
              (let* ((parts (split-string line "," t))
                     (hostname (string-trim (nth 0 parts)))
                     (username (if (> (length parts) 1)
                                   (string-trim (nth 1 parts))
                                 my/ssh-default-username)))
                (when (and hostname (not (string-empty-p hostname)))
                  (push (cons hostname username) servers)))))
          (forward-line 1))
        (nreverse servers)))))

;;==============================================================================
;; SERVER SELECTION WITH CONSULT
;;==============================================================================

(defun my/ssh-select-server ()
  "Interactively select a server from CSV using Consult.
Return (hostname . username) cons cell."
  (interactive)
  (require 'consult)  ; Ensure consult is loaded before using consult--read
  (let* ((servers (my/ssh-parse-csv-file my/ssh-server-csv-file))
         (choices (mapcar (lambda (server)
                            (format "%s (%s)" (car server) (cdr server)))
                          servers))
         (selected (when servers
                     (consult--read choices
                                    :prompt "Select server: "
                                    :require-match t
                                    :sort nil
                                    :category 'string
                                    :history 'my/ssh-history))))
    (when selected
      ;; Extract hostname from formatted string "hostname (username)"
      (let* ((hostname (car (split-string selected " (" t)))
             (username (substring (cadr (split-string selected "(" t)) 0 -1)))
        (cons hostname username)))))

;;==============================================================================
;; PERSPECTIVE INTEGRATION
;;==============================================================================

(defun my/ssh-create-server-perspective (hostname)
  "Create or switch to perspective named after HOSTNAME.
Replace dots with underscores for perspective name."
  (let ((persp-name (replace-regexp-in-string "\\." "_" hostname)))
    (my/persp-switch-or-create persp-name)
    persp-name))

;;==============================================================================
;; SSH CONNECTION HANDLING
;;==============================================================================

(defun my/ssh-connect-eshell (hostname username)
  "Open eshell with SSH connection to HOSTNAME as USERNAME.
Return the eshell buffer."
  (let ((default-directory "~/")  ; Start from home directory
        (ssh-command (format "ssh %s@%s" username hostname)))
    (eshell)
    ;; Wait for eshell to initialize
    (with-current-buffer "*eshell*"
      (goto-char (point-max))
      ;; Insert SSH command and execute
      (insert ssh-command)
      (eshell-send-input))
    (get-buffer "*eshell*")))

(defun my/ssh-connect-tramp (hostname username)
  "Open TRAMP connection to HOSTNAME as USERNAME.
Opens dired or shell on remote home directory via TRAMP SSH."
  (let ((tramp-path (format "/ssh:%s@%s:" username hostname)))
    (message "Opening TRAMP connection: %s" tramp-path)
    (cond
     ((eq my/ssh-tramp-mode 'dired)
      ;; Open dired on remote directory
      (dired tramp-path))
     ((eq my/ssh-tramp-mode 'shell)
      ;; Open shell with TRAMP default directory
      (let ((default-directory tramp-path))
        (shell)))
     ((eq my/ssh-tramp-mode 'dired-and-terminal)
      ;; Open dired on remote directory
      (dired tramp-path)
      ;; Then open shell with same TRAMP path
      (let ((default-directory tramp-path))
        (shell)))
     (t
      (error "Invalid my/ssh-tramp-mode: %s" my/ssh-tramp-mode)))))

;;==============================================================================
;; MAIN SSH SESSIONIZER FUNCTION
;;==============================================================================

(defun my/ssh-sessionizer ()
  "SSH sessionizer: select server, create perspective, open SSH connection.
Interactive function bound to C-S-f."
  (interactive)
  (let ((server (my/ssh-select-server)))
    (when server
      (let* ((hostname (car server))
             (username (cdr server))
             (persp-name (my/ssh-create-server-perspective hostname)))
        (message "Connecting to %s@%s in perspective %s" username hostname persp-name)
        ;; Check SSH key and prompt to copy if needed
        (my/ssh-check-key-copy hostname username)
        ;; Open SSH connection (TRAMP or eshell)
        (if my/ssh-use-tramp
            (my/ssh-connect-tramp hostname username)
          (my/ssh-connect-eshell hostname username))))))

;;==============================================================================
;; SSH KEY DETECTION (OPTIONAL)
;;==============================================================================

(defun my/ssh-check-key-copy (hostname username)
  "Check if SSH key needs to be copied to HOSTNAME for USERNAME.
If `my/ssh-prompt-for-key-copy' is non-nil, automatically copy key if SSH fails.
No prompt - assumes 'yes' for copying."
  (when my/ssh-prompt-for-key-copy
    (let ((default-directory "~/"))
      (condition-case err
          (if (zerop (call-process "ssh" nil nil nil "-o" "BatchMode=yes" "-q"
                                   (format "%s@%s" username hostname) "exit"))
              (message "SSH key already configured for %s@%s" username hostname)
            ;; SSH failed - automatically copy key (no prompt)
            (message "Copying SSH key to %s@%s..." username hostname)
            (shell-command (format "ssh-copy-id %s@%s" username hostname)))
        (error (message "SSH check failed: %s" (error-message-string err)))))))

;;==============================================================================
;; PROVIDE MODULE
;;==============================================================================

(provide 'my-ssh)
;;; my-ssh.el ends here