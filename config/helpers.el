;;; helpers.el --- Helper functions for Emacs configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Helper functions used throughout the configuration.

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
  "Run consult-ripgrep in current directory.
Uses regexp or fixed strings based on `my/search-use-regexp`."
  (interactive)
  (let ((args (if (boundp 'consult-ripgrep-args)
                  (if my/search-use-regexp
                      consult-ripgrep-args
                    (cons "--fixed-strings" consult-ripgrep-args))
                '("rg" "--null" "--line-buffered" "--color=never" "--max-columns=1000"
                  "--path-separator" "/" "--smart-case" "--no-heading" "--line-number"
                  "--hidden" "-g" "!.git/" "--" "."))))
    (let ((consult-ripgrep-args args))
      (consult-ripgrep (my/current-dir)))))

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

(provide 'helpers)
;;; helpers.el ends here