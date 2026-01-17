;;; git.el --- Git configuration (Magit) for Emacs

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
(defun my/magit-status-current-dir ()
  "Open magit status in current buffer's directory."
  (interactive)
  (let ((default-directory (my/current-dir)))
    (magit-status)))

;; Helper: Magit file dispatch (like lazygit)
(defun my/magit-dispatch ()
  "Open magit dispatch for quick Git operations.
Provides a lazygit-like interface with common actions."
  (interactive)
  (let ((default-directory (my/current-dir)))
    (magit-dispatch)))

;; Helper: Git blame current file
(defun my/magit-blame-current-file ()
  "Open magit blame for current file."
  (interactive)
  (when (buffer-file-name)
    (magit-blame)))

;; Helper: Git log current file
(defun my/magit-log-current-file ()
  "Open magit log for current file."
  (interactive)
  (when (buffer-file-name)
    (magit-log-current)))

;; Helper: Quick stage and commit
(defun my/magic-commit ()
  "Stage all changes and commit with a quick message."
  (interactive)
  (let ((default-directory (my/current-dir)))
    (magit-stage-modified)
    (magit-commit-create)))

;; Helper: Git push current branch
(defun my/magit-push-current ()
  "Push current branch to remote."
  (interactive)
  (let ((default-directory (my/current-dir)))
    (magit-push-current-to-pushremote nil)))

;; Helper: Git pull current branch
(defun my/magit-pull-current ()
  "Pull current branch from remote."
  (interactive)
  (let ((default-directory (my/current-dir)))
    (magit-pull-from-pushremote nil)))

;; Additional Git workflows
(defun my/magit-stash ()
  "Open magit stash interface."
  (interactive)
  (let ((default-directory (my/current-dir)))
    (magit-stash)))

(defun my/magit-branch ()
  "Open magit branch interface."
  (interactive)
  (let ((default-directory (my/current-dir)))
    (magit-branch)))

(defun my/magit-merge ()
  "Open magit merge interface."
  (interactive)
  (let ((default-directory (my/current-dir)))
    (magit-merge)))

(defun my/magit-rebase ()
  "Open magit rebase interface."
  (interactive)
  (let ((default-directory (my/current-dir)))
    (magit-rebase)))

(defun my/magit-fetch ()
  "Fetch from remote."
  (interactive)
  (let ((default-directory (my/current-dir)))
    (magit-fetch)))

(defun my/magit-remote ()
  "Manage remotes."
  (interactive)
  (let ((default-directory (my/current-dir)))
    (magit-remote)))

(provide 'git)
;;; git.el ends here