;;; languages.el --- Programming language mode configurations -*- lexical-binding: t -*-

;;; Commentary:
;; Configuration for programming language modes.

;;==============================================================================
;; PROGRAMMING LANGUAGES
;;==============================================================================

(use-package go-mode
  :ensure t
  :mode "\\.go\\'")

(use-package python-mode
  :ensure t
  :mode "\\.py\\'")

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :config
  (setq js2-basic-offset 2))

(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode)
         ("\\.css\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.tsx\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

(use-package cc-mode
  :ensure nil
  :mode (("\\.c\\'" . c-mode)
         ("\\.h\\'" . c-mode)
         ("\\.cpp\\'" . c++-mode)
         ("\\.hpp\\'" . c++-mode))
  :config
  (setq c-basic-offset 4))

;;==============================================================================
;; TREE-SITTER (Emacs 29+)
;;==============================================================================

;; Only enable tree-sitter if built-in support is available (Emacs 29+)
;; The "tree-sitter trio": built-in treesit, treesit-auto, tree-sitter-langs
(when (fboundp 'treesit-available-p)
  ;; tree-sitter-langs: Pre-built grammar bundles for many languages
  (use-package tree-sitter-langs
    :ensure t
    :defer t)
  
  ;; treesit-auto: Automatically install and use tree-sitter grammars
  (use-package treesit-auto
    :ensure t
    :after tree-sitter-langs
    :config
    (setq treesit-auto-install 'prompt
          treesit-auto-add-to-auto-mode-alist 'all
          treesit-auto-fallback-alist '((web-mode . (html-ts-mode css-ts-mode))))
    (global-treesit-auto-mode))
  
  ;; Configure specific tree-sitter modes with better defaults
  ;; Note: treesit-auto will automatically use tree-sitter modes when grammars are available
  
  ;; Keep web-mode for HTML/CSS files (but tree-sitter can handle them too)
  ;; We'll let treesit-auto decide based on grammar availability
  (setq web-mode-enable-engine-detection nil)
  
  ;; Optional: Customize tree-sitter settings for better performance
  (setq treesit-font-lock-level 4  ; Maximum syntax highlighting
        treesit-range-settings t))  ; Enable incremental parsing

(provide 'languages)
;;; languages.el ends here