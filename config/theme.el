;;; theme.el --- Theme configuration (Solarized) -*- lexical-binding: t -*-

;;; Commentary:
;; Solarized theme configuration with custom face settings.

;;==============================================================================
;; THEME & APPEARANCE
;;==============================================================================

(use-package solarized-theme
  :ensure t
  :config
  (setq solarized-distinct-fringe-background t
        solarized-use-variable-pitch nil
        solarized-scale-org-headlines nil)
  (load-theme 'solarized-dark t)
  
  ;; Pure black background
  (set-face-attribute 'default nil :background "#000000")
  (set-face-attribute 'fringe nil :background "#000000" :foreground "#586e75")
  
  ;; Yellow cursor
  (set-face-attribute 'cursor nil :background "#b58900")
  
  ;; Line numbers
  (set-face-attribute 'line-number nil 
                      :background "#000000" 
                      :foreground "#b58900"
                      :weight 'semi-bold)
  (set-face-attribute 'line-number-current-line nil 
                      :background "#000000" 
                      :foreground "#d33682"
                      :weight 'semi-bold)
  
  ;; Mode line
  (set-face-attribute 'mode-line nil 
                      :background "#073642"  ; Darker background for better contrast
                      :foreground "#fdf6e3"
                      :height 1.0
                      :underline nil
                      :overline nil
                      :box nil)
  (set-face-attribute 'mode-line-inactive nil 
                      :background "#002b36"  ; Darker for inactive
                      :foreground "#586e75"
                      :height 1.0
                      :underline nil
                      :overline nil
                      :box nil)
  
  ;; Window borders
  (set-face-attribute 'vertical-border nil :background "#000000" :foreground "#333333")
  (set-face-attribute 'window-divider nil :foreground "#333333")
  (set-face-attribute 'window-divider-first-pixel nil :foreground "#333333")
  (set-face-attribute 'window-divider-last-pixel nil :foreground "#333333")
  
  ;; Evil cursor colors
  (setq evil-normal-state-cursor '("#b58900" box)
        evil-insert-state-cursor '("#b58900" (bar . 7))
        evil-visual-state-cursor '("#b58900" box)
        evil-replace-state-cursor '("#dc322f" box)
        evil-operator-state-cursor '("#b58900" hollow)))

;;==============================================================================
;; DOOM MODELINE
;;==============================================================================

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 35
        doom-modeline-bar-width 6
        doom-modeline-window-width-limit 85
        doom-modeline-minor-modes nil
        doom-modeline-buffer-file-name-style 'auto
        doom-modeline-icon nil  ; Disable icons to avoid nerd-icons dependency
        doom-modeline-major-mode-color-icon nil
        doom-modeline-buffer-state-icon nil
        doom-modeline-buffer-modification-icon nil
        doom-modeline-unicode-fallback t
        doom-modeline-enable-word-count nil
        doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode)
        doom-modeline-indent-info nil
        doom-modeline-checker-simple-format t
        doom-modeline-vcs-max-length 12
        doom-modeline-lsp t
        doom-modeline-github nil
        doom-modeline-github-interval 300
        doom-modeline-modal-icon nil
        doom-modeline-modal-icon nil
        doom-modeline-modal t
        doom-modeline-modal-alist '((evil-emacs-state . " Emacs")
                                    (evil-insert-state . " Insert")
                                    (evil-normal-state . " Normal")
                                    (evil-visual-state . " Visual")
                                    (evil-motion-state . " Motion")
                                    (evil-operator-state . " Operator")
                                    (evil-replace-state . " Replace")))

  ;; Customize doom-modeline faces for better visibility with Solarized
  (set-face-attribute 'doom-modeline-bar nil :background "#073642" :underline nil :overline nil :box nil)
  (set-face-attribute 'doom-modeline-bar-inactive nil :background "#002b36" :underline nil :overline nil :box nil)
  
  ;; Ensure text is visible on dark backgrounds
  (set-face-attribute 'doom-modeline-project-dir nil 
                      :foreground "#93a1a1"  ; Light gray
                      :weight 'bold)
  (set-face-attribute 'doom-modeline-buffer-file nil 
                      :foreground "#fdf6e3"  ; Light cream
                      :weight 'bold)
  (set-face-attribute 'doom-modeline-buffer-modified nil 
                      :foreground "#dc322f"  ; Red for modified
                      :weight 'bold)
  (set-face-attribute 'doom-modeline-buffer-major-mode nil 
                      :foreground "#2aa198"  ; Cyan for major mode
                      :weight 'bold)
  (set-face-attribute 'doom-modeline-info nil 
                      :foreground "#859900"  ; Green for info
                      :weight 'bold)
  (set-face-attribute 'doom-modeline-warning nil 
                      :foreground "#b58900"  ; Yellow for warnings
                      :weight 'bold)
  (set-face-attribute 'doom-modeline-urgent nil 
                      :foreground "#dc322f"  ; Red for urgent
                      :weight 'bold))

(provide 'theme)
;;; theme.el ends here