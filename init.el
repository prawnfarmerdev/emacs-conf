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
;; Add local packages directory
(add-to-list 'load-path (expand-file-name "local-packages" user-emacs-directory))

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

;; Test configuration for ripgrep debugging (set to t to enable)
(defvar my/enable-test-ripgrep nil
  "If non-nil, load test-ripgrep.el with debugging functions.")
(when my/enable-test-ripgrep
  (load "test-ripgrep"))

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
 '(custom-safe-themes
   '("7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5"
     "36d4b9573ed57b3c53261cb517eef2353058b7cf95b957f691f5ad066933ae84"
     "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773"
     "48d34b6afe72407ca494387c8bea495bb2deee96bd88516f302db1f11e1810a1"
     "5f78a36d69bb8df702a8f6ef8dd523da044050872d3ab9bbc265dbe250d4b0e4"
     "d2e24f1b34ee66ffaf9b3c9d76242b5dfa85d2baf932b5c3ae706f509aa8c560"
     "7fea145741b3ca719ae45e6533ad1f49b2a43bf199d9afaee5b6135fd9e6f9b8"
     default))
 '(package-selected-packages
   '(color-theme-sanityinc-solarized embark-consult evil-collection gcmh
                                     general go-mode helpful js2-mode
                                     magit-delta magit-todos
                                     marginalia markdown-mode
                                     mustard-theme orderless
                                     perspective python-mode
                                     solarized-gruvbox-theme
                                     solarized-theme tree-sitter-langs
                                     treesit-auto vertico web-mode)))
 (custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  )
