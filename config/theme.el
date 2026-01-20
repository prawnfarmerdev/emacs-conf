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


(provide 'theme)
;;; theme.el ends here