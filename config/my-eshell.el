;;; my-eshell.el --- Minimal eshell configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Streamlined eshell setup with essential features only.

(require 'em-hist)
(require 'em-alias)

;;==============================================================================
;; CORE FUNCTIONALITY
;;==============================================================================

;; Open eshell in current directory
(defun my/eshell-here ()
  "Open eshell in current buffer's directory."
  (interactive)
  (let ((default-directory (or default-directory "~/")))
    (eshell)))

;;==============================================================================
;; ESSENTIAL SETTINGS
;;==============================================================================

;; History
(setq eshell-history-size 10000
      eshell-hist-ignoredups t
      eshell-save-history-on-exit t)

;; Completion
(setq eshell-cmpl-ignore-case t
      eshell-cmpl-cycle-completions nil
      completion-auto-help t)

;; Scrolling
(setq eshell-scroll-to-bottom-on-input 'all
      eshell-scroll-to-bottom-on-output 'all)

;;==============================================================================
;; USEFUL ALIASES
;;==============================================================================

(setq eshell-command-aliases-list
      '(("ll" "ls -l")
        ("la" "ls -la")
        (".." "cd ..")
        ("..." "cd ../..")
        ("cls" "clear")))

;;==============================================================================
;; KEYBINDINGS
;;==============================================================================

(defun my/eshell-setup ()
  "Configure eshell keybindings."
  ;; Disable evil-mode if present
  (when (fboundp 'evil-local-mode)
    (evil-local-mode -1))
  
  ;; Essential shell shortcuts
  (define-key eshell-mode-map (kbd "C-a") 'eshell-bol)
  (define-key eshell-mode-map (kbd "C-d") 'eshell-delchar-or-maybe-eof)
  (define-key eshell-mode-map (kbd "C-u") 'eshell-kill-input)
  (define-key eshell-mode-map (kbd "C-w") 'backward-kill-word)
  
  ;; History search if consult is available
  (when (fboundp 'consult-history)
    (define-key eshell-mode-map (kbd "C-r") 
      (lambda () (interactive) (consult-history eshell-history-ring)))))

(add-hook 'eshell-mode-hook #'my/eshell-setup)

(provide 'my-eshell)
;;; my-eshell.el ends here
