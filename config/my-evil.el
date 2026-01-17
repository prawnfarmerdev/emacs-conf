;;; my-evil.el --- Evil mode (Vim keybindings) configuration

;;; Commentary:
;; Evil mode configuration for Vim-style keybindings.

;;==============================================================================
;; EVIL MODE - VIM KEYBINDINGS
;;==============================================================================

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-undo-system 'undo-redo)
  :config
  (evil-mode 1)
  
  ;; Ctrl-c exits to normal mode
  (define-key evil-insert-state-map (kbd "C-c") 'evil-normal-state)
  (define-key evil-visual-state-map (kbd "C-c") 'evil-normal-state)
  (define-key evil-replace-state-map (kbd "C-c") 'evil-normal-state)
  
  ;; Ctrl-Shift-c for copy
  (define-key evil-visual-state-map (kbd "C-S-c") 'evil-yank)
  (define-key evil-normal-state-map (kbd "C-S-c") 'evil-yank-line)
  
  (setq-default indicate-empty-lines t
                indicate-buffer-boundaries nil))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (setq evil-collection-mode-list 
        (delq 'ibuffer (delq 'eshell evil-collection-mode-list)))
  (evil-collection-init))

;; Ensure evil stays in emacs state for tabulated-list-mode (used by perspective switcher)
(with-eval-after-load 'evil
  (evil-set-initial-state 'tabulated-list-mode 'emacs))

;; Ensure evil stays in emacs state when entering tabulated-list-mode
(add-hook 'tabulated-list-mode-hook
          (lambda ()
            (when (boundp 'evil-state)
              (evil-emacs-state))))

(provide 'my-evil)
;;; my-evil.el ends here