;;; init.el --- Emacs Configuration with Tmux/Vim-style keybindings (Modular) -*- lexical-binding: t -*-

;;; Commentary:
;; Tmux-style buffer management with Vim keybindings
;; C-f: Fuzzy find directories (like fzf)
;; C-SPC /: Search text in current directory (toggle regex/fixed with C-SPC g)
;;
;; Modular configuration split into separate files in ~/.emacs.d/config/

;;==============================================================================
;; STARTUP TIMING
;;==============================================================================

(defvar my/start-time (current-time)
  "Time when Emacs started loading configuration.")

(defun my/display-startup-time ()
  "Display startup time and any other performance metrics."
  (let ((elapsed (float-time (time-subtract (current-time) my/start-time))))
    (message "Emacs configuration loaded successfully in %.2f seconds" elapsed)))

;;==============================================================================
;; MODULE LOADER
;;==============================================================================

;; Add config directory to load path
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

;; Feature flags (set to nil to disable modules)
(defvar my/enable-git t
  "If non-nil, load Git configuration (Magit).")
(defvar my/enable-ssh t
  "If non-nil, load SSH sessionizer.")
(defvar my/enable-terminal-enhancements t
  "If non-nil, load enhanced terminal configuration.")

;; Load modules in dependency order
(load "core")         ; Package management, UI, editor behavior, fonts
(load "my-evil")      ; Evil mode & evil-collection  
(load "completion")   ; Vertico, orderless, consult, embark, marginalia
(load "helpers")      ; Helper functions (directory, search, terminal)
(when my/enable-git
  (load "git"))          ; Git configuration (Magit)
(when my/enable-terminal-enhancements
  (load "my-eshell"))    ; Enhanced eshell configuration
(load "my-perspective")  ; Workspace management with custom switcher
(load "my-tab-line")   ; Tab-bar integration with perspective
(load "my-navigation") ; Navigation with consult (replaces FZF)
(when my/enable-ssh
  (load "my-ssh"))       ; SSH sessionizer with consult and perspective
(load "keybindings")  ; Tmux-style keybindings (depends on helpers)
(load "theme")        ; Solarized theme & face customizations
(load "languages")    ; Programming language modes

;; Platform-specific configuration (loaded after all modules to override settings)
(when (bound-and-true-p my/is-windows)
  (load "windows"))

;; Display startup time in batch mode (since emacs-startup-hook may not run)
(when noninteractive
  (my/display-startup-time))

;;==============================================================================
;; INITIALIZATION HOOKS
;;==============================================================================

;; Add any initialization that needs to run after all modules are loaded
(add-hook 'emacs-startup-hook #'my/display-startup-time)

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
  '(package-selected-packages '(vertico vertico-multiform orderless consult embark embark-consult marginalia helpful general perspective doom-modeline magit-delta magit-todos solarized-theme cc-mode python-mode web-mode markdown-mode js2-mode go-mode evil evil-collection which-key gcmh magit)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
