;;; languages.el --- Programming language mode configurations -*- lexical-binding: t -*-

;;; Commentary:
;; Configuration for programming language modes.

;;==============================================================================
;; PROGRAMMING LANGUAGES
;;==============================================================================

(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :defer t
  :hook (go-ts-mode . (lambda () (require 'go-mode)))
  :config
  (when (fboundp 'go-mode-setup)
    (add-hook 'go-ts-mode-hook 'go-mode-setup)))

(use-package python-mode
  :ensure t
  :mode "\\.py\\'"
  :defer t
  :hook (python-ts-mode . (lambda () (require 'python-mode)))
  :config
  (when (fboundp 'python-mode)
    (add-hook 'python-ts-mode-hook 'python-mode-hook)))

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :defer t
  :hook (markdown-ts-mode . (lambda () (require 'markdown-mode)))
  :config
  (when (fboundp 'markdown-mode)
    (add-hook 'markdown-ts-mode-hook 'markdown-mode-hook)))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :defer t
  :hook (js-ts-mode . (lambda () (require 'js2-mode)))
  :config
  (when (fboundp 'js2-mode)
    (add-hook 'js-ts-mode-hook 'js2-mode-hook))
  (add-hook 'js-ts-mode-hook (lambda () (setq js2-basic-offset 2))))

(use-package web-mode
  :ensure t
  :defer t
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
  :defer t
  :config
  (setq c-basic-offset 4)
  (add-hook 'c-ts-mode-hook (lambda () (c-set-style "gnu") (setq c-basic-offset 4)))
  (add-hook 'c++-ts-mode-hook (lambda () (c-set-style "gnu") (setq c-basic-offset 4))))

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
    :defer t
    :after tree-sitter-langs
    :config
    (setq treesit-auto-install 'prompt
          treesit-auto-add-to-auto-mode-alist 'all)
    (global-treesit-auto-mode))
  
  ;; Keep web-mode for HTML/CSS files while allowing tree-sitter for JS/TS
  ;; treesit-auto will add tree-sitter modes, but web-mode can still be used manually
  (setq web-mode-enable-engine-detection nil)
  
  ;; Optional: Customize tree-sitter settings for better performance
  (setq treesit-font-lock-level 4  ; Maximum syntax highlighting
        treesit-range-settings t))  ; Enable incremental parsing

(provide 'languages)
;;; languages.el ends here