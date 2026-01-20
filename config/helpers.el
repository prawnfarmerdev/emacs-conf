;;; helpers.el --- Helper functions for Emacs configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Helper functions used throughout the configuration.

(require 'shell)

;;==============================================================================
;; HELPER FUNCTIONS
;;==============================================================================

(defun my/current-dir ()
  "Return directory of current buffer, or `default-directory`."
  (if-let ((file (buffer-file-name)))
      (file-name-directory file)
    default-directory))

;;;###autoload
(defun my/open-current-dir-dired ()
  "Open dired in current buffer's directory."
  (interactive)
  (dired (my/current-dir)))

(defvar my/search-use-regexp t
  "If non-nil, use regexp search with consult-ripgrep.
If nil, use fixed strings search.")

;;;###autoload
(defun my/consult-ripgrep-toggle-regex ()
  "Toggle between regexp and fixed string search for consult-ripgrep."
  (interactive)
  (setq my/search-use-regexp (not my/search-use-regexp))
  (message "Search mode: %s" (if my/search-use-regexp "regexp" "fixed strings")))

;;;###autoload
(defun my/consult-ripgrep-current-dir ()
  "Run consult-ripgrep in current directory with pattern prompt first.
Prompts for search pattern, then runs consult-ripgrep with pattern pre-filled.
Avoids empty interface by ensuring pattern is entered before consult starts."
  (interactive)
  (require 'consult)
  
  (let ((dir (my/current-dir))
        (pattern (read-string "Search: ")))
    
    (when (string-blank-p pattern)
      (user-error "Search pattern cannot be empty"))
    
    (setq dir (expand-file-name dir))
    (unless (file-directory-p dir)
      (user-error "Directory does not exist: %s" dir))
    
    ;; Handle fixed string search by inserting --fixed-strings after command name
    (let ((consult-ripgrep-args
           (if my/search-use-regexp
               consult-ripgrep-args
             ;; For fixed strings, add --fixed-strings after the command name (rg or rg.exe)
             (if (listp consult-ripgrep-args)
                 (let ((cmd (car consult-ripgrep-args))
                       (rest (cdr consult-ripgrep-args)))
                   (cons cmd (cons "--fixed-strings" rest)))
               ;; If it's a string, add --fixed-strings after first word
               (if (string-match "^\\(\\S-+\\)\\(.*\\)" consult-ripgrep-args)
                   (concat (match-string 1 consult-ripgrep-args)
                           " --fixed-strings"
                           (match-string 2 consult-ripgrep-args))
                 (concat "--fixed-strings " consult-ripgrep-args))))))
      (condition-case err
          (consult-ripgrep dir pattern)
        (error
         (message "consult-ripgrep error: %s" err)
         (user-error "Search failed: %s" (error-message-string err)))))))

;;;###autoload
(defun my/consult-find-current-dir ()
  "Run consult-find in current directory for fuzzy file search."
  (interactive)
  (consult-find (my/current-dir)))

;;==============================================================================
;; PROJECT MANAGEMENT
;;==============================================================================

;;;###autoload
(defun my/create-new-project ()
  "Create new project directory with git and GitHub setup.
Asks for project name, creates directory in ~/projects/, initializes git,
creates README.md, and optionally creates GitHub repository using gh CLI."
  (interactive)
  ;; Check if gh CLI is available
  (unless (executable-find "gh")
    (message "Warning: GitHub CLI (gh) not found. GitHub repository creation will be skipped."))
  
  (let* ((project-name (read-string "Project name: "))
         (project-dir (expand-file-name (concat "~/projects/" project-name)))
         (create-github (and (executable-find "gh")
                             (y-or-n-p "Create GitHub repository? ")))
         (visibility (when create-github
                       (if (y-or-n-p "Make repository public? (No for private) ")
                           "--public"
                         "--private"))))
    
    ;; Validate project name
    (when (string-empty-p project-name)
      (error "Project name cannot be empty"))
    
    ;; Check if directory already exists
    (when (file-exists-p project-dir)
      (if (y-or-n-p (format "Directory %s already exists. Use it? " project-dir))
          (message "Using existing directory: %s" project-dir)
        (error "Project directory already exists")))
    
    ;; Create directory
    (make-directory project-dir t)
    (message "Created directory: %s" project-dir)
    
    ;; Change to project directory
    (let ((default-directory project-dir))
      ;; Initialize git repository if not already
      (unless (file-exists-p (expand-file-name ".git" project-dir))
        (message "Initializing git repository...")
        (unless (zerop (call-process-shell-command "git init -b main"))
          (error "Failed to initialize git repository"))
        
        ;; Create README.md
        (with-temp-file (expand-file-name "README.md" project-dir)
          (insert (format "# %s\n\nProject description\n" project-name)))
        (message "Created README.md")
        
        ;; Initial commit
        (unless (zerop (call-process-shell-command "git add README.md"))
          (error "Failed to stage README.md"))
        (unless (zerop (call-process-shell-command "git commit -m 'initial commit'"))
          (error "Failed to create initial commit"))
        (message "Created initial commit")
        
        ;; Create GitHub repository if requested
        (when create-github
          (message "Creating GitHub repository...")
          (let ((command (format "gh repo create %s %s --source=. --remote=origin --push" 
                                 project-name visibility)))
            (message "Running: %s" command)
            (let ((exit-code (call-process-shell-command command)))
              (if (zerop exit-code)
                  (message "GitHub repository created successfully: %s" project-name)
                (message "GitHub repository creation may have failed (exit code: %d)" exit-code))))
          ;; Wait a moment for push to complete
          (sleep-for 1)))
      
      ;; Open the new project directory in dired
      (dired project-dir)
      (message "Project created: %s" project-dir))))

;;;###autoload
(defun my/detect-shell ()
  "Return appropriate shell command for current platform."
  (cond
   ((memq system-type '(windows-nt ms-dos cygwin))
    (if (executable-find "powershell.exe") "powershell.exe" "cmd.exe"))
   ((executable-find "pwsh") "pwsh")
   (t "/bin/bash")))

(defun my/shell-args (shell-name)
  "Return shell arguments for SHELL-NAME.
SHELL-NAME can be a string or nil. Returns nil for unknown shells."
  (when (stringp shell-name)
    (cond
     ((string-match "powershell\\.exe\\|pwsh" shell-name)
      '("-NoExit" "-NoLogo" "-NoProfile" "-Command" "-"))
     ((string-match "cmd\\.exe" shell-name)
      '("/k"))
     (t nil))))

(defun my/configure-shell-mode ()
  "Configure shell mode for specific shell types.
Sets comint variables for better PowerShell and general shell experience."
  (when (derived-mode-p 'shell-mode)
    ;; Skip configuration for PowerShell buffers created by powershell.el
    ;; as they have their own configuration
    (unless (string-match "\\*PowerShell\\*" (buffer-name))
      (when (stringp explicit-shell-file-name)
        (let ((shell-name (file-name-nondirectory explicit-shell-file-name)))
          (cond
           ((string-match "powershell\\.exe\\|pwsh" shell-name)
            ;; PowerShell specific settings
            (setq-local comint-process-echoes nil)  ; PowerShell echoes its own prompt
            (setq-local comint-use-prompt-regexp t)
            (setq-local comint-prompt-regexp "^PS.*> "))
           ((string-match "cmd\\.exe" shell-name)
            ;; cmd.exe settings
            (setq-local comint-process-echoes t)
            (setq-local comint-use-prompt-regexp t)
            (setq-local comint-prompt-regexp "^[A-Z]:\\.*?> "))
           (t
            ;; Unix shell defaults
            (setq-local comint-process-echoes nil)
            (setq-local comint-use-prompt-regexp nil))))))))

;; Run configuration when shell mode starts
(with-eval-after-load 'shell
  (add-hook 'shell-mode-hook #'my/configure-shell-mode))

;;;###autoload
(defun my/open-shell-here ()
  "Open shell in current buffer's directory with PowerShell detection.
On Windows, uses powershell.el if available, otherwise powershell.exe.
On Linux/macOS, uses pwsh if available, otherwise falls back to bash."
  (interactive)
  (let ((default-directory (my/current-dir)))
    (cond
     ;; Use powershell.el function if available and on Windows
     ((memq system-type '(windows-nt ms-dos cygwin))
      (condition-case err
          (progn
            ;; Try to load powershell.el if not already loaded
            (unless (fboundp 'powershell)
              (require 'powershell))
            (powershell))
        (error
         ;; Fall back to generic shell if powershell.el fails
         (message "powershell.el failed: %s, falling back to generic shell" 
                  (error-message-string err))
         (let ((explicit-shell-file-name (my/detect-shell))
               (explicit-shell-args (my/shell-args explicit-shell-file-name)))
           (shell)))))
     ;; Otherwise use generic shell with detected shell
     (t
      (let ((explicit-shell-file-name (my/detect-shell))
            (explicit-shell-args (my/shell-args explicit-shell-file-name)))
        (shell))))))

;;==============================================================================
;; ANSI-TERM TERMINAL EMULATOR
;;==============================================================================

;;;###autoload
(defun my/open-ansi-term-here ()
  "Open ansi-term (terminal emulator) in current buffer's directory.
Uses PowerShell on Windows, bash on Unix. Provides better terminal emulation
than shell-mode for SSH and interactive programs."
  (interactive)
  (let ((default-directory (my/current-dir))
        (shell (cond
                ((eq system-type 'windows-nt)
                 (if (executable-find "powershell.exe") 
                     "powershell.exe"
                   "cmd.exe"))
                (t
                 (or (executable-find "bash") "/bin/bash")))))
    (ansi-term shell)))

;; Configure term-mode for Windows SSH compatibility
(defun my/configure-term-mode-windows ()
  "Configure term-mode for Windows SSH compatibility."
  (when (derived-mode-p 'term-mode)
    (when (eq system-type 'windows-nt)
      ;; Set up term-mode for better terminal emulation
      (setq-local term-prompt-regexp "^[A-Z]:\\.*?> \\|^PS.*> ")
      (setq-local term-escape-char ?\C-c)
      ;; Note: term-mode provides better pseudo-terminal emulation than shell-mode
      (message "term-mode: For SSH on Windows, use 'ssh -t hostname'"))))

(add-hook 'term-mode-hook #'my/configure-term-mode-windows)

;;==============================================================================
;; EAT TERMINAL EMULATOR
;;==============================================================================

;;;###autoload
(defun my/open-eat-here ()
  "Open eat (Emulated Advanced Terminal) in current buffer's directory.
Eat is a terminal emulator with excellent Windows and SSH support.
On Windows, provides proper shell arguments to avoid spawn errors."
  (interactive)
  (let ((default-directory (my/current-dir))
        (shell (cond
                ((eq system-type 'windows-nt)
                 (if (executable-find "powershell.exe")
                     "powershell.exe"
                   "cmd.exe"))
                (t
                 (or (executable-find "bash") "/bin/bash"))))
        (shell-args (cond
                     ((eq system-type 'windows-nt)
                      (if (executable-find "powershell.exe")
                          '("-NoExit" "-NoLogo" "-NoProfile")
                        '("/k")))
                     (t
                      nil))))
    (condition-case err
        (progn
          ;; Ensure eat is loaded
          (require 'eat nil t)
          (if shell-args
              ;; Use eat-make to pass shell arguments
              (eat-make (generate-new-buffer-name "*eat*") shell nil shell-args)
            ;; No arguments, use plain eat
            (eat shell)))
      (error
       (message "eat failed: %s, falling back to ansi-term" (error-message-string err))
       (my/open-ansi-term-here)))))

;;;###autoload
(defun my/open-eat-eshell-here ()
  "Open eshell with eat terminal emulation enabled.
This provides excellent terminal emulation within eshell, especially
for Windows SSH support."
  (interactive)
  (let ((default-directory (my/current-dir)))
    ;; Ensure eat is loaded
    (if (require 'eat nil t)
        (progn
          ;; Enable eat-eshell-mode globally if not already enabled
          (unless (and (boundp 'eat-eshell-mode) eat-eshell-mode)
            (eat-eshell-mode 1))
          ;; Open eshell
          (eshell t))
      ;; If eat is not available, fall back to regular eshell
      (message "eat package not available, using regular eshell")
      (eshell t))))

;; Configure eat for Windows SSH compatibility
(defun my/configure-eat-mode-windows ()
  "Configure eat-mode for Windows SSH compatibility."
  (when (derived-mode-p 'eat-mode)
    (when (eq system-type 'windows-nt)
      ;; Eat provides excellent terminal emulation, SSH should work well
      (setq-local eat-term-prompt-regexp "^[A-Z]:\\.*?> \\|^PS.*> ")
      ;; Eat handles pseudo-terminal allocation better than shell-mode
      (message "eat-mode: SSH should work without -t flag on Windows"))))

(with-eval-after-load 'eat
  (add-hook 'eat-mode-hook #'my/configure-eat-mode-windows)
  
  ;; Windows workaround for eat's hardcoded /usr/bin/env sh -c command
  (when (eq system-type 'windows-nt)
    ;; Define advice function
    (defun my/eat-exec-windows-advice (orig-fun buffer name command startfile switches)
      "Adjust eat-exec arguments for Windows compatibility."
      (if (string= command "/usr/bin/env")
          ;; Replace Unix-style command with Windows shell
          (let* ((shell (if (executable-find "powershell.exe")
                            "powershell.exe"
                          "cmd.exe"))
                 (args (if (executable-find "powershell.exe")
                           '("-NoExit" "-NoLogo" "-NoProfile")
                         '("/k"))))
            (funcall orig-fun buffer name shell startfile args))
        ;; Otherwise call original
        (funcall orig-fun buffer name command startfile switches)))
    ;; Add advice only once
    (unless (advice-member-p #'my/eat-exec-windows-advice 'eat-exec)
      (advice-add 'eat-exec :around #'my/eat-exec-windows-advice))))

;;==============================================================================
;; UNIFIED TERMINAL FUNCTION
;;==============================================================================

;;;###autoload
(defun my/open-best-terminal-here ()
  "Open the best available terminal in current directory.
Tries: eat -> vterm (Unix only) -> ansi-term -> powershell.el -> shell."
  (interactive)
  (let ((default-directory (my/current-dir)))
    (cond
     ;; eat (recommended for Windows SSH compatibility)
     ((fboundp 'eat)
      (my/open-eat-here))
     
     ;; vterm (Unix/Linux only - not recommended for Windows)
     ((and (not (eq system-type 'windows-nt))
           (fboundp 'vterm))
      (vterm))
     
     ;; ansi-term (built-in terminal emulator)
     ((fboundp 'ansi-term)
      (my/open-ansi-term-here))
     
     ;; Windows: try powershell.el first
     ((and (eq system-type 'windows-nt)
           (condition-case nil
               (progn
                 (unless (fboundp 'powershell)
                   (require 'powershell))
                 t)
             (error nil)))
      (powershell))
     
     ;; Fall back to shell
     (t
      (my/open-shell-here)))))

(provide 'helpers)
;;; helpers.el ends here