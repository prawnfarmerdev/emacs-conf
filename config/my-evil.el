;;; my-evil.el --- Evil mode (Vim keybindings) configuration -*- lexical-binding: t -*-

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
  

  
  ;; Ctrl-Shift-c for copy
  (define-key evil-visual-state-map (kbd "C-S-c") 'evil-yank)
  (define-key evil-normal-state-map (kbd "C-S-c") 'evil-yank-line)
  
  (setq-default indicate-empty-lines t
                indicate-buffer-boundaries nil)
  
  ;; Disable evil in eshell buffers
  (add-to-list 'evil-buffer-regexps '("\\*eshell\\*")))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (setq evil-collection-mode-list 
        (delq 'ibuffer (delq 'eshell evil-collection-mode-list)))
  (evil-collection-init))

;; Ensure evil stays in emacs state for tabulated-list-mode (used by ibuffer and other list buffers)
(with-eval-after-load 'evil
  (evil-set-initial-state 'tabulated-list-mode 'emacs)
  (evil-set-initial-state 'eshell-mode 'emacs))

;; Ensure evil stays in emacs state when entering tabulated-list-mode
(add-hook 'tabulated-list-mode-hook
          (lambda ()
            (when (boundp 'evil-state)
              (evil-emacs-state))))

;; Fix TAB key in org-mode to work with org-tempo templates
(with-eval-after-load 'evil
  (with-eval-after-load 'org
    ;; Unbind TAB from evil-jump-forward in org-mode buffers
    (define-key evil-motion-state-map (kbd "TAB") nil)
    (define-key evil-normal-state-map (kbd "TAB") nil)
    ;; Ensure TAB triggers org-cycle (which calls org-tempo via hook)
    (evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)
    (evil-define-key 'insert org-mode-map (kbd "TAB") 'org-cycle)
    (evil-define-key 'visual org-mode-map (kbd "TAB") 'org-cycle)
    (evil-define-key 'motion org-mode-map (kbd "TAB") 'org-cycle)))

(provide 'my-evil)
;;; my-evil.el ends here