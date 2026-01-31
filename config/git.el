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
  :hook (magit-mode . magit-delta-mode))

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