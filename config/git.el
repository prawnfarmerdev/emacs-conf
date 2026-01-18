;;; git.el --- Git configuration (Magit) for Emacs -*- lexical-binding: t -*-

;;; Commentary:
;; Magit configuration for Git operations with lazygit-like keybindings.

;;==============================================================================
;; MAGIT - GIT INTERFACE
;;==============================================================================

(use-package magit
  :ensure t
  :defer t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-diff-refine-hunk 'all)
  (magit-save-repository-buffers 'dontask)
  (magit-process-popup-time 10)
  (magit-commit-show-diff nil))

(use-package magit-delta
  :ensure t
  :after magit
  :hook (magit-mode . magit-delta-mode)
  :config
  (setq magit-delta-default-dark-theme "solarized-dark"))

(use-package magit-todos
  :ensure t
  :after magit
  :config
  (magit-todos-mode))

;; Helper: Open magit status in current directory
;;;###autoload
(defun my/magit-status-current-dir ()
  "Open magit status in current buffer's directory."
  (interactive)
  (let ((default-directory (my/current-dir)))
    (magit-status)))

;; Helper: Magit file dispatch (like lazygit)
;;;###autoload
(defun my/magit-dispatch ()
  "Open magit dispatch for quick Git operations.
Provides a lazygit-like interface with common actions."
  (interactive)
  (let ((default-directory (my/current-dir)))
    (magit-dispatch)))

;; Helper: Git blame current file
;;;###autoload
(defun my/magit-blame-current-file ()
  "Open magit blame for current file."
  (interactive)
  (when (buffer-file-name)
    (magit-blame)))

;; Helper: Git log current file
;;;###autoload
(defun my/magit-log-current-file ()
  "Open magit log for current file."
  (interactive)
  (when (buffer-file-name)
    (magit-log-current)))

;; Helper: Quick stage and commit
;;;###autoload
(defun my/magic-commit ()
  "Stage all changes and commit with a quick message."
  (interactive)
  (let ((default-directory (my/current-dir)))
    (magit-stage-modified)
    (magit-commit-create)))

;; Helper: Git push current branch
;;;###autoload
(defun my/magit-push-current ()
  "Push current branch to remote."
  (interactive)
  (let ((default-directory (my/current-dir)))
    (magit-push-current-to-pushremote nil)))

;; Helper: Git pull current branch
;;;###autoload
(defun my/magit-pull-current ()
  "Pull current branch from remote."
  (interactive)
  (let ((default-directory (my/current-dir)))
    (magit-pull-from-pushremote nil)))

;; Additional Git workflows
;;;###autoload
(defun my/magit-stash ()
  "Open magit stash interface."
  (interactive)
  (let ((default-directory (my/current-dir)))
    (magit-stash)))

;;;###autoload
(defun my/magit-branch ()
  "Open magit branch interface."
  (interactive)
  (let ((default-directory (my/current-dir)))
    (magit-branch)))

;;;###autoload
(defun my/magit-merge ()
  "Open magit merge interface."
  (interactive)
  (let ((default-directory (my/current-dir)))
    (magit-merge)))

;;;###autoload
(defun my/magit-rebase ()
  "Open magit rebase interface."
  (interactive)
  (let ((default-directory (my/current-dir)))
    (magit-rebase)))

;;;###autoload
(defun my/magit-fetch ()
  "Fetch from remote."
  (interactive)
  (let ((default-directory (my/current-dir)))
    (magit-fetch)))

;;;###autoload
(defun my/magit-remote ()
  "Manage remotes."
  (interactive)
  (let ((default-directory (my/current-dir)))
    (magit-remote)))

;;==============================================================================
;; GITHUB INTEGRATION
;;==============================================================================

;;;###autoload
(defun my/open-github-pr ()
  "Open GitHub compare page for current branch in browser.
Uses git remote URL and current branch to construct GitHub URL."
  (interactive)
  (let ((default-directory (my/current-dir)))
    (unless (locate-dominating-file default-directory ".git")
      (error "Not in a git repository"))
    
    (let* ((remote-url (string-trim (shell-command-to-string "git config --get remote.origin.url")))
           (branch (string-trim (shell-command-to-string "git rev-parse --abbrev-ref HEAD")))
           (github-url remote-url))
      
      (when (string-empty-p remote-url)
        (error "No remote 'origin' configured"))
      
      (when (string-empty-p branch)
        (error "Could not determine current branch"))
      
      ;; Convert SSH URL to HTTPS if needed
      (cond
       ((string-match "^git@github\\.com:\\(.+\\)\\.git$" github-url)
        (setq github-url (concat "https://github.com/" (match-string 1 github-url))))
       ((string-match "^git@github\\.com:\\(.+\\)$" github-url)  ; Without .git
        (setq github-url (concat "https://github.com/" (match-string 1 github-url))))
       ((string-match "^https://github\\.com/\\(.+\\)\\.git$" github-url)
        (setq github-url (replace-regexp-in-string "\\.git$" "" github-url)))
       ((not (string-match "github\\.com" github-url))
        (error "Remote URL is not a GitHub repository: %s" github-url)))
      
      ;; Construct compare URL
      (let ((compare-url (concat github-url "/compare/" branch)))
        (message "Opening GitHub compare: %s" compare-url)
        (browse-url compare-url)))))

(provide 'git)
;;; git.el ends here