;;; my-ssh.el --- SSH sessionizer with consult and perspective integration -*- lexical-binding: t -*-

;;; Commentary:
;; SSH sessionizer functionality that:
;; 1. Reads server inventory from SSH config file (~/.ssh/config)
;; 2. Provides interactive server selection using Consult
;; 3. Creates a perspective named after the selected server
;; 4. Opens TRAMP connection (dired and/or shell)
;; 5. Supports MFA configuration via SSH config options

;;==============================================================================
;; CONFIGURATION VARIABLES
;;==============================================================================

(defvar my/ssh-config-file (expand-file-name "~/.ssh/config")
  "Path to SSH config file containing server inventory.
SSH config format with Host, HostName, User, IdentityFile, etc.
If file doesn't exist, it will be created with a template.")

(defvar my/ssh-default-username (user-login-name)
  "Default username to use if not specified in SSH config.")

(defvar my/ssh-auto-generate-config t
  "If non-nil, automatically generate SSH config template if file doesn't exist.")

(defvar my/ssh-use-tramp t
  "If non-nil, use TRAMP for remote connections instead of eshell SSH.
TRAMP provides better integration with Emacs buffers and files.")

(defvar my/ssh-tramp-mode 'dired-and-terminal
  "TRAMP connection mode: `dired', `shell', or `dired-and-terminal'.
When `my/ssh-use-tramp' is non-nil, determines what to open:
- `dired': Open dired on remote home directory
- `shell': Open shell buffer connected via TRAMP
- `dired-and-terminal': Open dired then a terminal with same TRAMP path")

;;==============================================================================
;; SSH CONFIG PARSING
;;==============================================================================

(defun my/ssh-ensure-config-file ()
  "Ensure SSH config file exists, create template if needed.
Returns path to config file."
  (let ((config-file my/ssh-config-file))
    (when (and my/ssh-auto-generate-config
               (not (file-exists-p config-file)))
      (make-directory (file-name-directory config-file) t)
      (with-temp-file config-file
        (insert "# SSH Configuration for Emacs sessionizer
# Add your servers here using standard SSH config format
#
# UNCOMMENTED EXAMPLE (remove # to use):
# Host myserver
#     HostName server.example.com
#     User myusername
#     IdentityFile ~/.ssh/id_rsa
#     # MFA options (if needed):
#     # PreferredAuthentications publickey,keyboard-interactive
#     # AuthenticationMethods publickey,keyboard-interactive
#
# Host anotherserver
#     HostName 192.168.1.100
#     User admin
#
# The 'Host' alias will be used for selection in Emacs.
# If HostName is not specified, the Host alias is used as the hostname.
# If User is not specified, your default username will be used.

# ACTIVE EXAMPLE (uncommented - will appear in server selection):
Host example
    HostName 192.168.2.82
    User root
    # Uncomment and configure if you have SSH key:
    # IdentityFile ~/.ssh/id_rsa

# Add your own servers below this line:\n")))
    config-file))

(defun my/ssh-parse-config-file (_config-file)
  "Parse SSH config file and return alist of (hostname . username).
Extracts Host entries with their HostName and User settings.
If HostName is not specified, uses Host alias as hostname.
If User is not specified, uses `my/ssh-default-username'.
The _CONFIG-FILE argument is ignored; uses `my/ssh-config-file' variable."
  (let ((config-file (my/ssh-ensure-config-file)))
    (when (file-exists-p config-file)
      (with-temp-buffer
        (insert-file-contents config-file)
        (goto-char (point-min))
        (let (servers
              current-host current-hostname current-user)
          (while (not (eobp))
            (let* ((raw-line (buffer-substring (line-beginning-position) (line-end-position)))
                   (line (string-trim raw-line)))
              (unless (or (string-empty-p line)
                          (string-prefix-p "#" line))
                (let* ((words (split-string line))
                       (first-word (downcase (car words))))
                  (cond
                   ((string= first-word "host")
                    ;; Save previous host block if any
                    (when current-host
                      (push (cons (or current-hostname current-host)
                                  (or current-user my/ssh-default-username))
                            servers))
                    ;; Start new block
                    (setq current-host (cadr words)
                          current-hostname nil
                          current-user nil))
                   ((string= first-word "hostname")
                    (setq current-hostname (cadr words)))
                   ((string= first-word "user")
                    (setq current-user (cadr words)))
                   ;; Ignore other directives
                   ))))
            (forward-line 1))
          ;; Save last host block
          (when current-host
            (push (cons (or current-hostname current-host)
                        (or current-user my/ssh-default-username))
                  servers))
          (nreverse servers))))))

;;==============================================================================
;; SERVER SELECTION WITH CONSULT
;;==============================================================================

(defun my/ssh-select-server ()
  "Interactively select a server from SSH config using Consult.
Return (hostname . username) cons cell.
If no servers found, prompts to edit config file."
  (interactive)
  (require 'consult)  ; Ensure consult is loaded before using consult--read
  (let* ((servers (my/ssh-parse-config-file my/ssh-config-file))
         (choices (mapcar (lambda (server)
                            (format "%s (%s)" (car server) (cdr server)))
                          servers)))
    (cond
     (servers
      (let ((selected (consult--read choices
                                     :prompt "Select server: "
                                     :require-match t
                                     :sort nil
                                     :category 'string
                                     :history 'my/ssh-history)))
        (when selected
          ;; Extract hostname from formatted string "hostname (username)"
          (let* ((hostname (car (split-string selected " (" t)))
                 (username (substring (cadr (split-string selected "(" t)) 0 -1)))
            (cons hostname username)))))
     (t
      ;; No servers found
      (message "No SSH hosts found in %s" my/ssh-config-file)
      (when (y-or-n-p (format "No SSH hosts configured. Edit %s? " my/ssh-config-file))
        (find-file my/ssh-config-file))
      nil))))

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
        ;; Open SSH connection (TRAMP or eshell)
        (if my/ssh-use-tramp
            (my/ssh-connect-tramp hostname username)
          (my/ssh-connect-eshell hostname username))))))

;;==============================================================================

;;==============================================================================
;; PROVIDE MODULE
;;==============================================================================

(provide 'my-ssh)
;;; my-ssh.el ends here