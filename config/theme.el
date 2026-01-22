;;; theme.el --- Menudo theme (black iackgrounds, gray text, blue highlights) -*- lexical-binding: t -*-

;;; Commentary:
;; Menudo theme - A custom dark theme featuring:
;; - Pure black backgrounds for code and line numbers
;; - Dim gray text (#666666) for readability
;; - Yellow cursor with Evil mode cursor colors
;; - Blue mode-line with black text, red buffer names
;; - Green minibuffer selection (vertico-current)
;; - Red parentheses matching (show-paren-match)
;; - Git branch colors (green local, blue remote, amber current)
;; - Custom tab-line with black background and orange selected tab
;;
;; Originally based on Fleury theme by Shams Parvez Arka:
;; https://github.com/ShamsParvezArka/fleury-theme.el
;;
;; Note: Theme auto-disables in terminal mode (emacs -nw), using basic black background and gray text instead.

;;==============================================================================
;; MENUDO THEME
;;==============================================================================

(deftheme menudo "Menudo theme with black backgrounds, gray text, and blue highlights")

;; Color palette
(let* ((light-bronze       "#666666")     ; Dim gray instead of #b99468
       (charcoal-gray-lite "#1e1e1e")
       (amber-gold         "#fcaa05")
       (medium-gray        "#404040")
       (dim-gray           "#666666")
       (goldenrod          "#f0c674")
       (bright-orange      "#ffaa00")
       (dusty-rose         "#dc7575")
       (sunflower-yellow   "#edb211")
       (burnt-orange       "#de451f")
       (sky-blue           "#2895c7")
       (bright-red         "#dc322f")
       (fresh-green        "#66bc11")
       (lime-green         "#003939")
       (vivid-vermilion    "#f0500c")
       (golden-yellow      "#f0bb0c")
       (pure-black         "#000000")
       (dusty-sage         "#9ba290")
       (coffee-brown       "#63523d")
       (tab-selected-color  "#9a5201")
       
       (mode-line-foreground-active pure-black)
       (mode-line-background-active sky-blue)
       (mode-line-border            "#161616")
       )

  (custom-theme-set-faces
   'menudo

   ;; UI Elements
   `(default           ((t (:background ,pure-black :foreground ,light-bronze))))  ; Pure black background
   `(cursor            ((t (:background "#b58900"))))  ; Yellow cursor
   `(region            ((t (:background ,lime-green))))
   `(highlight         ((t (:background ,charcoal-gray-lite))))
   `(fringe            ((t (:background ,pure-black))))  ; Pure black fringe
   `(vertical-border   ((t (:foreground ,pure-black))))  ; Black window borders
   `(shadow            ((t (:foreground ,medium-gray :background ,pure-black))))  ; Tilde lines at end of buffer
   `(minibuffer-prompt ((t (:foreground ,amber-gold :weight bold))))

   ;; Line Numbers - Pure black backgrounds
   `(line-number              ((t (:foreground ,medium-gray :background ,pure-black))))
   `(line-number-current-line ((t (:background ,pure-black :foreground ,light-bronze))))
   
   ;; HL Line - background handled after theme load

   ;; Font Lock Faces
   `(font-lock-comment-face       ((t (:foreground ,dim-gray))))
   `(font-lock-keyword-face       ((t (:foreground ,goldenrod))))
   `(font-lock-string-face        ((t (:foreground ,bright-orange))))
   `(font-lock-constant-face      ((t (:foreground ,bright-orange))))
   `(font-lock-builtin-face       ((t (:foreground ,dusty-rose))))
   `(font-lock-preprocessor-face  ((t (:foreground ,dusty-rose))))
   `(font-lock-type-face          ((t (:foreground ,sunflower-yellow))))
   `(font-lock-function-name-face ((t (:foreground ,burnt-orange))))
   `(font-lock-variable-name-face ((t (:foreground ,light-bronze))))
   `(font-lock-variable-use-face  ((t (:foreground ,sky-blue))))
   `(font-lock-preprocessor-face  ((t (:foreground ,dusty-rose))))
   `(font-lock-warning-face       ((t (:foreground ,bright-red :weight bold))))
   `(font-lock-doc-face           ((t (:foreground ,fresh-green))))

    ;; Mode Line
    `(mode-line          ((t (:background ,mode-line-background-active :foreground ,mode-line-foreground-active :box (:line-width 1 :color ,mode-line-border :style nil)))))
    `(mode-line-inactive ((t (:background ,dim-gray :foreground ,light-bronze :box (:line-width 1 :color ,mode-line-border :style nil)))))  ; Pure black background with gray text
    `(mode-line-buffer-id ((t (:foreground ,bright-orange :weight bold))))
    
    ;; Git / Magit
    `(magit-branch-local  ((t (:foreground ,fresh-green))))
    `(magit-branch-remote ((t (:foreground ,sky-blue))))
    `(magit-branch-current ((t (:foreground ,amber-gold :weight bold))))
    `(vc-mode             ((t (:foreground ,amber-gold))))
    
     ;; Search & String Matching
   `(match           ((t (:background ,golden-yellow   :foreground ,pure-black))))
   `(isearch         ((t (:background ,vivid-vermilion :foreground ,pure-black))))
   `(lazy-highlight  ((t (:background ,golden-yellow   :foreground ,pure-black))))
   `(ido-first-match ((t (:foreground ,golden-yellow))))
   `(ido-only-match  ((t (:foreground ,vivid-vermilion))))
   
    ;; Minibuffer & Completion
     `(vertico-current            ((t (:background ,lime-green :foreground ,dim-gray))))
    `(orderless-match-face-0     ((t (:foreground ,golden-yellow :weight bold))))
    `(orderless-match-face-1     ((t (:foreground ,sky-blue :weight bold))))
    `(orderless-match-face-2     ((t (:foreground ,fresh-green :weight bold))))
    `(orderless-match-face-3     ((t (:foreground ,dusty-rose :weight bold))))
    `(completions-first-difference ((t (:foreground ,amber-gold :weight bold))))
    `(completions-common-part    ((t (:foreground ,light-bronze))))
   
   ;; Custom Elements
    `(show-paren-match    ((t (:background ,vivid-vermilion))))
    `(show-paren-mismatch ((t (:background ,dusty-sage))))

   ;; Tooltip and Popup
   `(tooltip ((t (:background ,coffee-brown :foreground ,amber-gold))))

   ;; Compilation
   `(flycheck-error             ((t (:underline (:color ,bright-red :style wave)))))
   `(compilation-error          ((t (:foreground ,bright-red))))
   `(compilation-info           ((t ,(list :foreground fresh-green  :inherit 'unspecified))))
   `(compilation-warning        ((t ,(list :foreground coffee-brown :bold t       :inherit 'unspecified))))
    `(compilation-mode-line-fail ((t ,(list :foreground bright-red   :weight 'bold :inherit 'unspecified))))
    `(compilation-mode-line-exit ((t ,(list :foreground fresh-green  :weight 'bold :inherit 'unspecified))))
    
    ;; Tab Line
    `(tab-line              ((t (:background ,pure-black))))
    `(tab-line-tab-current  ((t (:foreground ,tab-selected-color :weight bold))))
    `(tab-line-tab-inactive ((t (:foreground ,light-bronze))))
    ))

;; Add directory to custom theme load path
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'menudo)

;; Enable the theme only in graphical mode
(when (display-graphic-p)
  (enable-theme 'menudo)
  (message "Menudo theme enabled (graphical mode)"))

;; Evil cursor colors (override theme's cursor for Evil states)
(setq evil-normal-state-cursor '("#b58900" box)
      evil-insert-state-cursor '("#b58900" (bar . 7))
      evil-visual-state-cursor '("#b58900" box)
      evil-replace-state-cursor '("#dc322f" box)
      evil-operator-state-cursor '("#b58900" hollow))

(require 'hl-line)
(set-face-attribute 'hl-line nil :background "#000000")

;; Keep Fleury's auxiliary hooks
(add-hook 'prog-mode-hook 'hl-line-mode)
(setq-default cursor-in-non-selected-windows nil)
(setq-default cursor-type 'box)

(defun custom/update-cursor-type ()
  (setq cursor-type
        (if (derived-mode-p 'prog-mode 'text-mode)
            '(bar . 4)
          'box)))

(add-hook 'post-command-hook 'custom/update-cursor-type)

(provide 'theme)
;;; theme.el ends here
