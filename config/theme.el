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
(let* ((almost-white       "#cccccc")     ; Brighter gray for default text (was #999999)
       (charcoal-gray-lite "#1e1e1e")
       (amber-gold         "#fcaa05")
       (medium-gray        "#404040")
       (dim-gray           "#666666")
       (goldenrod          "#f0c674")
       (bright-orange      "#ffaa00")
       (dusty-rose         "#dc7575")
       (sunflower-yellow   "#edb211")
       (burnt-orange       "#de451f")
       (sky-blue           "#268bd2")
;;       (dim-blue           "#073642")
       (bright-red         "#dc322f")
       (fresh-green        "#66bc11")
       (lime-green         "#003939")
       (vivid-vermilion    "#f0500c")
       (golden-yellow      "#f0bb0c")
       (pure-black         "#000000")
       (dusty-sage         "#9ba290")
       (coffee-brown       "#63523d")
       (bellpepper-green   "#355e3b")

       (mode-line-foreground-active almost-white)
       (mode-line-background-active bellpepper-green)
       (mode-line-border            "#161616")
       )

  (custom-theme-set-faces
   'menudo

   ;; UI Elements
   `(default           ((t (:background ,pure-black :foreground ,almost-white))))  ; Pure black background
   `(cursor            ((t (:background "#b58900"))))  ; Yellow cursor
   `(region            ((t (:background ,lime-green))))
   `(highlight         ((t (:background ,charcoal-gray-lite))))
   `(fringe            ((t (:background ,pure-black))))  ; Pure black fringe
   `(vertical-border   ((t (:foreground ,pure-black))))  ; Black window borders
   `(shadow            ((t (:foreground ,medium-gray :background ,pure-black))))  ; Tilde lines at end of buffer
   `(minibuffer-prompt ((t (:foreground ,amber-gold :weight bold))))

   ;; Line Numbers - Pure black backgrounds
   `(line-number              ((t (:foreground ,medium-gray :background ,pure-black))))
   `(line-number-current-line ((t (:background ,pure-black :foreground ,almost-white))))

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
    `(font-lock-variable-name-face ((t (:foreground ,almost-white))))
    `(font-lock-variable-use-face  ((t (:foreground ,sky-blue))))
    `(font-lock-warning-face       ((t (:foreground ,bright-red :weight bold))))
    `(font-lock-doc-face           ((t (:foreground ,fresh-green))))

    ;; Mode Line
    `(mode-line          ((t (:background ,mode-line-background-active :foreground ,mode-line-foreground-active :box (:line-width 1 :color ,mode-line-border :style nil)))))
    `(mode-line-inactive ((t (:background ,dim-gray :foreground ,almost-white :box (:line-width 1 :color ,mode-line-border :style nil)))))  ; Pure black background with gray text
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
     `(vertico-current            ((t (:background ,lime-green :foreground ,almost-white))))
    `(orderless-match-face-0     ((t (:foreground ,golden-yellow :weight bold))))
    `(orderless-match-face-1     ((t (:foreground ,sky-blue :weight bold))))
    `(orderless-match-face-2     ((t (:foreground ,fresh-green :weight bold))))
    `(orderless-match-face-3     ((t (:foreground ,dusty-rose :weight bold))))
    `(completions-first-difference ((t (:foreground ,amber-gold :weight bold))))
    `(completions-common-part    ((t (:foreground ,almost-white))))

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

    ;; LSP & Completion
    `(eglot-highlight-symbol-face ((t (:background ,charcoal-gray-lite))))
    `(eglot-diagnostic-error-face ((t (:underline (:color ,bright-red :style wave)))))
    `(eglot-diagnostic-warning-face ((t (:underline (:color ,amber-gold :style wave)))))
    `(eglot-diagnostic-note-face ((t (:underline (:color ,sky-blue :style wave)))))
    `(eglot-diagnostic-hint-face ((t (:underline (:color ,fresh-green :style wave)))))
    `(corfu-current ((t (:background ,lime-green :foreground ,pure-black))))
    `(corfu-default ((t (:background ,charcoal-gray-lite))))
    `(corfu-bar ((t (:background ,sky-blue))))
    `(corfu-border ((t (:background ,medium-gray))))
    ;; Rainbow delimiters
    `(rainbow-delimiters-depth-1-face ((t (:foreground ,sky-blue))))
    `(rainbow-delimiters-depth-2-face ((t (:foreground ,fresh-green))))
    `(rainbow-delimiters-depth-3-face ((t (:foreground ,amber-gold))))
    `(rainbow-delimiters-depth-4-face ((t (:foreground ,dusty-rose))))
    `(rainbow-delimiters-depth-5-face ((t (:foreground ,goldenrod))))
    `(rainbow-delimiters-depth-6-face ((t (:foreground ,sunflower-yellow))))
    `(rainbow-delimiters-depth-7-face ((t (:foreground ,bright-orange))))
    `(rainbow-delimiters-depth-8-face ((t (:foreground ,vivid-vermilion))))

     ))

;; Add directory to custom theme load path
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'menudo)

;; Enable the theme in graphical mode or daemon mode (for clients)
(when (or (display-graphic-p) (daemonp))
  (enable-theme 'menudo)
  (message "Menudo theme enabled (graphical/daemon mode)"))

;; Evil cursor colors (override theme's cursor for Evil states)
;; Make cursor thicker overall, visual thicker than insert
(when (boundp 'evil-normal-state-cursor)
  (setq evil-normal-state-cursor '("#b58900" box)
        evil-insert-state-cursor '("#b58900" (bar . 6))
        evil-visual-state-cursor '("#b58900" (bar . 12))
        evil-replace-state-cursor '("#dc322f" box)
        evil-operator-state-cursor '("#b58900" hollow)))

(require 'hl-line)
(set-face-attribute 'hl-line nil :background "#000000")

;; Keep Fleury's auxiliary hooks
(add-hook 'prog-mode-hook 'hl-line-mode)
(setq-default cursor-in-non-selected-windows nil)
(setq-default cursor-type 'box)

;; Disabled - interferes with Evil cursor states
;; (defun custom/update-cursor-type ()
;;   (setq cursor-type
;;         (if (derived-mode-p 'prog-mode 'text-mode)
;;             '(bar . 4)
;;           'box)))
;;
;; (add-hook 'post-command-hook 'custom/update-cursor-type)

;; Ensure theme is enabled for new graphical frames (especially in daemon mode)
(defun menudo-enable-if-needed (&optional frame)
  "Enable Menudo theme if not already enabled and FRAME is graphical."
  (with-selected-frame (or frame (selected-frame))
    (when (and (display-graphic-p)
               (not (memq 'menudo custom-enabled-themes)))
      (enable-theme 'menudo)
      (message "Menudo theme enabled for new graphical frame"))))

(add-hook 'after-make-frame-functions #'menudo-enable-if-needed)

(provide 'theme)
;;; theme.el ends here
