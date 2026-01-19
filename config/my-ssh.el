;;; my-ssh.el --- SSH sessionizer with consult and perspective integration -*- lexical-binding: t -*-

;;; Commentary:
;; SSH sessionizer functionality that:
;; 1. Reads server inventory from CSV file (~/.emacs.d/data/servers.csv) as primary source
;; 2. Provides interactive server selection using Consult with display names
;; 3. Creates a perspective named after the selected server
;; 4. Opens TRAMP connection (dired and/or shell)
;; 5. Generates SSH config entries on-demand with key management
;; 6. Supports MFA configuration via SSH config options
;; 
;; CSV format: hostname,name,username  (name optional, can be empty)
;; - hostname: Server hostname or IP address (required)
;; - name: Display name for easier searching in selection (optional, can be empty)
;; - username: SSH username for the server (required)
;; Also supports 2-column format: hostname,username
;; 
;; If CSV file doesn't exist, falls back to SSH config file parsing.

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

(defvar my/ssh-csv-file (expand-file-name "~/.emacs.d/data/servers.csv")
  "Path to CSV file containing server inventory.
CSV format: hostname,name,username (name optional, can be empty)
- hostname: Server hostname or IP address (required)
- name: Display name for selection (optional, can be empty)
- username: SSH username for the server (required)
Also supports 2-column format: hostname,username
If file doesn't exist, CSV parsing will be skipped.")

(defvar my/ssh-default-key nil
  "Default SSH private key path (e.g., ~/.ssh/id_rsa).
If nil, automatically detect existing keys or generate new one.")

(defvar my/ssh-auto-generate-key t
  "If non-nil, generate SSH key pair if none exists.
Only applies when `my/ssh-default-key' is nil and no keys found.")

(defvar my/ssh-csv-as-primary-source t
  "If non-nil, use CSV file as primary server source.
If CSV file exists, servers will be read from CSV and SSH config
entries will be generated on-demand when servers are selected.
If CSV file doesn't exist, fall back to SSH config parsing.")

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
;; CSV PARSING AND KEY MANAGEMENT
;;==============================================================================
(defun my/ssh-parse-csv-file (csv-file)
  "Parse CSV file and return alist of (display . (hostname . username)).
CSV format: hostname,name,username (name optional, can be empty)
Also supports 2-column format: hostname,username
Lines starting with # are ignored.
Returns empty list if file doesn't exist or no valid entries."
  (when (file-exists-p csv-file)
    (with-temp-buffer
      (insert-file-contents csv-file)
      (goto-char (point-min))
      (let (servers)
        (while (not (eobp))
          (let* ((raw-line (buffer-substring (line-beginning-position) (line-end-position)))
                 (line (string-trim raw-line)))
            (unless (or (string-empty-p line)
                        (string-prefix-p "#" line))
              (let* ((parts (split-string line "," nil))  ; keep empty columns
                     (nparts (length parts)))
                (cond
                 ((>= nparts 3)
                  (let* ((hostname (string-trim (nth 0 parts)))
                         (name (string-trim (nth 1 parts)))
                         (username (string-trim (nth 2 parts))))
                    (when (and (not (string-empty-p hostname))
                               (not (string-empty-p username)))
                      (let ((display (if (string-empty-p name)
                                         hostname
                                       (format "%s (%s)" name hostname))))
                        (push (cons display
                                    (cons hostname username))
                              servers)))))
                 ((= nparts 2)
                  (let* ((hostname (string-trim (nth 0 parts)))
                         (username (string-trim (nth 1 parts))))
                    (when (and (not (string-empty-p hostname))
                               (not (string-empty-p username)))
                      (push (cons hostname
                                  (cons hostname username))
                            servers))))
                 (t nil)))))  ; ignore lines with <2 columns
          (forward-line 1))
        (nreverse servers)))))

(defun my/ssh-find-ssh-keys ()
  "Find existing SSH private keys in ~/.ssh/.
Returns list of key paths (without .pub extension)."
  (let ((ssh-dir (expand-file-name "~/.ssh/"))
        (keys ()))
    (when (file-exists-p ssh-dir)
      (dolist (file (directory-files ssh-dir t))
        (when (and (not (file-directory-p file))
                   (string-match "\\.pub$" file))
          (let ((priv-key (replace-regexp-in-string "\\.pub$" "" file)))
            (when (file-exists-p priv-key)
              (push priv-key keys))))))
    keys))

(defun my/ssh-ensure-ssh-key ()
  "Ensure an SSH key exists, return private key path.
If `my/ssh-default-key' is non-nil, use that.
Otherwise, check for existing keys, or generate new one if `my/ssh-auto-generate-key'."
  (cond
   (my/ssh-default-key
    (expand-file-name my/ssh-default-key))
   (t
    (let ((keys (my/ssh-find-ssh-keys)))
      (cond
       (keys
        (car keys))  ; Use first found key
       (my/ssh-auto-generate-key
        (my/ssh-generate-key))  ; TODO: implement key generation
       (t
        nil))))))

(defun my/ssh-generate-key ()
  "Generate SSH key pair.
Returns private key path.
Generates RSA 4096 key in ~/.ssh/. If id_rsa already exists,
generates id_rsa_emacs instead."
  (let* ((key-dir (expand-file-name "~/.ssh/"))
         (base-name "id_rsa")
         (priv-key (expand-file-name base-name key-dir))
         (attempt 0)
         (max-attempts 5))
    (unless (file-exists-p key-dir)
      (make-directory key-dir t))
    ;; Find available key name
    (while (and (file-exists-p priv-key) (< attempt max-attempts))
      (setq attempt (1+ attempt))
      (setq base-name (format "id_rsa_emacs%s" (if (> attempt 1) (format "_%d" attempt) "")))
      (setq priv-key (expand-file-name base-name key-dir)))
    (when (file-exists-p priv-key)
      (error "SSH key generation failed: all candidate key names exist in %s" key-dir))
    ;; Generate key using ssh-keygen
    (message "Generating SSH key %s..." priv-key)
    (call-process "ssh-keygen" nil nil nil "-t" "rsa" "-b" "4096" "-f" priv-key "-N" "")
    (if (file-exists-p priv-key)
        (progn
          (message "SSH key generated: %s" priv-key)
          priv-key)
      (error "SSH key generation failed"))))

(defun my/ssh-config-has-host? (hostname)
  "Check if SSH config already has entry for HOSTNAME."
  (let ((config-file (my/ssh-ensure-config-file)))
    (when (file-exists-p config-file)
      (with-temp-buffer
        (insert-file-contents config-file)
        (goto-char (point-min))
        (re-search-forward (format "^Host\\s-+%s\\(\\s-\\|$\\)" (regexp-quote hostname)) nil t)))))

(defun my/ssh-add-config-entry (hostname username &optional key-path)
  "Add SSH config entry for HOSTNAME with USERNAME and optional KEY-PATH.
Appends to config file."
  (let ((config-file (my/ssh-ensure-config-file))
        (entry (format "\nHost %s\n    HostName %s\n    User %s%s\n"
                       hostname hostname username
                       (if key-path (format "\n    IdentityFile %s" key-path) ""))))
    (with-temp-buffer
      (when (file-exists-p config-file)
        (insert-file-contents config-file))
      (goto-char (point-max))
      ;; Ensure we end with newline
      (unless (bolp) (insert "\n"))
      (insert entry)
      (write-region (point-min) (point-max) config-file))))


;;==============================================================================
;; SERVER SELECTION WITH CONSULT
;;==============================================================================

(defun my/ssh-select-server ()
  "Interactively select a server from CSV or SSH config using Consult.
Return (hostname . username) cons cell.
Primary source is CSV file if `my/ssh-csv-as-primary-source' is non-nil
and CSV file exists. Otherwise uses SSH config file.
If CSV is used, ensures SSH config entries exist (generates if missing)."
  (interactive)
  (require 'consult)
  (let* ((csv-file my/ssh-csv-file)
         (use-csv (and my/ssh-csv-as-primary-source
                       (file-exists-p csv-file)))
         (servers
          (if use-csv
              ;; Parse CSV and ensure SSH config entries
              (let ((csv-servers (my/ssh-parse-csv-file csv-file)))
                (when csv-servers
                  (let ((key-path (my/ssh-ensure-ssh-key)))
                    (dolist (server csv-servers)
                      (let ((hostname (car (cdr server)))
                            (username (cdr (cdr server))))
                        (unless (my/ssh-config-has-host? hostname)
                          (my/ssh-add-config-entry hostname username key-path))))))
                csv-servers)
            ;; Fall back to SSH config parsing
            (my/ssh-parse-config-file my/ssh-config-file)))
         (choices (mapcar (lambda (server)
                            ;; server is either (display . (hostname . username)) from CSV
                            ;; or (hostname . username) from SSH config
                            (if (consp (cdr server))  ; CSV format
                                (car server)          ; display name already formatted
                              (format "%s (%s)" (car server) (cdr server))))
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
          ;; Extract hostname and username based on source
          (if use-csv
              ;; Find the server entry in CSV list
              (let ((server (assoc selected servers)))
                (when server
                  (cdr server)))  ; returns (hostname . username)
            ;; SSH config format
            (let* ((hostname (car (split-string selected " (" t)))
                   (username (substring (cadr (split-string selected "(" t)) 0 -1)))
              (cons hostname username))))))
     (t
      ;; No servers found
      (message "No SSH hosts found in %s" (if use-csv csv-file my/ssh-config-file))
      (when (y-or-n-p (format "No SSH hosts configured. Edit %s? "
                              (if use-csv csv-file my/ssh-config-file)))
        (find-file (if use-csv csv-file my/ssh-config-file)))
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