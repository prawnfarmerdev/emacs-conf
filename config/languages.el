;;; languages.el --- Programming language mode configurations -*- lexical-binding: t -*-

;;; Commentary:
;; Complete configuration for programming languages with LSP, completion, and snippets.

;;==============================================================================
;; SNIPPETS
;;==============================================================================

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  ;; Disable default tab key to avoid conflicts
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  ;; Use a different key for snippet expansion
  (define-key yas-minor-mode-map (kbd "C-c y") 'yas-expand))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

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

(when (fboundp 'treesit-available-p)
  ;; tree-sitter-langs: Pre-built grammar bundles
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

  ;; Tree-sitter settings
  (setq treesit-font-lock-level 4
        treesit-range-settings t))

;;==============================================================================
;; LSP-MODE: Robust LSP client
;;==============================================================================

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook
  ((python-mode python-ts-mode) . lsp-deferred)
  ((go-mode go-ts-mode) . lsp-deferred)
  ((js-mode js-ts-mode typescript-mode typescript-ts-mode tsx-ts-mode) . lsp-deferred)
  ((c-mode c-ts-mode c++-mode c++-ts-mode) . lsp-deferred)
  ((rust-mode rust-ts-mode) . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)

  :init
   (setq lsp-keymap-prefix "C-c L")

  :config
  ;; Performance settings
  (setq lsp-idle-delay 0.5
        lsp-log-io nil
        read-process-output-max (* 1024 1024))

  ;; Core settings
  (setq lsp-auto-guess-root t
        lsp-restart 'auto-restart
        lsp-keep-workspace-alive nil
        lsp-enable-snippet t  ; Now enabled with yasnippet
        lsp-enable-xref t
        lsp-enable-imenu t)

  ;; CRITICAL: Let corfu handle completion
  (setq lsp-completion-provider :none)

  ;; UI settings
  (setq lsp-headerline-breadcrumb-enable nil
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable t
        lsp-signature-auto-activate t
        lsp-signature-render-documentation t)

  ;; File watching
  (setq lsp-enable-file-watchers t
        lsp-file-watch-threshold 2000)

  ;; Disable invasive features
  (setq lsp-enable-on-type-formatting nil
        lsp-enable-indentation nil)

  ;; Language-specific settings
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t)
     ("gopls.usePlaceholders" t t)))

  ;; Keybindings
  (define-key lsp-mode-map (kbd "C-c L a") 'lsp-execute-code-action)
  (define-key lsp-mode-map (kbd "C-c L r") 'lsp-rename)
  (define-key lsp-mode-map (kbd "C-c L f") 'lsp-format-buffer)
  (define-key lsp-mode-map (kbd "C-c L d") 'lsp-describe-thing-at-point)
  (define-key lsp-mode-map (kbd "C-c L i") 'lsp-find-implementation)
  (define-key lsp-mode-map (kbd "C-c L t") 'lsp-find-type-definition)
  (define-key lsp-mode-map (kbd "C-c L =") 'lsp-format-region)
  (define-key lsp-mode-map (kbd "C-c L o") 'lsp-organize-imports))

;;==============================================================================
;; LSP-UI: Enhanced UI for lsp-mode
;;==============================================================================

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-delay 0.2

        lsp-ui-peek-enable t
        lsp-ui-peek-always-show t

        lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-delay 0.5
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-max-width 80
        lsp-ui-doc-max-height 30)

  (define-key lsp-ui-mode-map (kbd "C-c L .") 'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map (kbd "C-c L ?") 'lsp-ui-peek-find-references))

;;==============================================================================
;; LSP-TREEMACS: Symbol browser
;;==============================================================================

(use-package lsp-treemacs
  :ensure t
  :after (lsp-mode treemacs)
  :commands (lsp-treemacs-errors-list
             lsp-treemacs-symbols
             lsp-treemacs-references)
  :config
  (lsp-treemacs-sync-mode 1)
  (define-key lsp-mode-map (kbd "C-c L e") 'lsp-treemacs-errors-list)
  (define-key lsp-mode-map (kbd "C-c L S") 'lsp-treemacs-symbols))

;;==============================================================================
;; FLYCHECK: Syntax checking
;;==============================================================================

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-display-errors-delay 0.3
        flycheck-highlighting-mode 'lines
        flycheck-indication-mode 'left-fringe)

  (setq flycheck-display-errors-function
        #'flycheck-display-error-messages-unless-error-list)

  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;;==============================================================================
;; CORFU: Modern completion
;;==============================================================================

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)

  :config
  (setq corfu-auto t
        corfu-auto-delay 0.1
        corfu-auto-prefix 1
        corfu-cycle t
        corfu-preselect 'prompt
        corfu-quit-at-boundary 'separator
        corfu-quit-no-match 'separator
        corfu-preview-current nil
        corfu-min-width 20
        corfu-max-width 100
        corfu-count 10
        corfu-scroll-margin 4)

  (define-key corfu-map (kbd "TAB") 'corfu-next)
  (define-key corfu-map (kbd "<tab>") 'corfu-next)
  (define-key corfu-map (kbd "S-TAB") 'corfu-previous)
  (define-key corfu-map (kbd "<backtab>") 'corfu-previous)
  (define-key corfu-map (kbd "RET") 'corfu-insert)
  (define-key corfu-map (kbd "M-d") 'corfu-info-documentation)
  (define-key corfu-map (kbd "M-l") 'corfu-info-location))

(use-package corfu-popupinfo
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :config
  (setq corfu-popupinfo-delay '(0.5 . 0.2))
  (define-key corfu-map (kbd "M-d") 'corfu-popupinfo-toggle)
  (define-key corfu-map (kbd "M-n") 'corfu-popupinfo-scroll-up)
  (define-key corfu-map (kbd "M-p") 'corfu-popupinfo-scroll-down))

;;==============================================================================
;; CAPE: Completion backends
;;==============================================================================

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)

  :config
  (setq cape-dabbrev-check-other-buffers t
        cape-dabbrev-min-length 2)

  ;; Integrate with LSP
  (defun my/lsp-setup-completion ()
    "Setup completion for LSP buffers."
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'lsp-completion-at-point
                       #'cape-dabbrev
                       #'cape-file))))

  (add-hook 'lsp-completion-mode-hook #'my/lsp-setup-completion)

  ;; For non-LSP prog-mode
  (add-hook 'prog-mode-hook
            (lambda ()
              (unless (or (bound-and-true-p lsp-mode)
                         (bound-and-true-p eglot--managed-mode))
                (setq-local completion-at-point-functions
                           (list #'cape-dabbrev
                                 #'cape-file
                                 #'cape-keyword))))))

;;==============================================================================
;; LISP CONFIGURATION
;;==============================================================================

(use-package rainbow-delimiters
  :ensure t
  :hook ((emacs-lisp-mode lisp-mode common-lisp-mode scheme-mode clojure-mode) . rainbow-delimiters-mode)
  :config
  (setq rainbow-delimiters-max-face-count 8))

;;==============================================================================
;; WHITESPACE CONFIGURATION (Fixed - no red highlighting)
;;==============================================================================

;; Disable intrusive global whitespace highlighting
(global-whitespace-mode -1)
(setq-default show-trailing-whitespace nil)

;; Configure whitespace-mode to be minimal and helpful
(use-package whitespace
  :ensure nil
  :diminish whitespace-mode
  :config
  ;; Only show actual problems
  (setq whitespace-style '(face trailing empty))

  ;; Subtle colors
  (set-face-attribute 'whitespace-trailing nil
                      :background "gray25"
                      :foreground 'unspecified)

  ;; Auto-cleanup trailing whitespace on save
  (add-hook 'before-save-hook #'delete-trailing-whitespace))

;;==============================================================================
;; DIAGNOSTIC & HELPER FUNCTIONS
;;==============================================================================



(defun my/fix-lsp-and-completion ()
  "Fix LSP and completion in current buffer."
  (interactive)

  (require 'lsp-mode nil t)
  (require 'corfu nil t)
  (require 'cape nil t)

  (unless (bound-and-true-p global-corfu-mode)
    (global-corfu-mode 1))

  (when (and (derived-mode-p 'prog-mode)
             (buffer-file-name)
             (not (bound-and-true-p lsp-mode)))
    (lsp-deferred))

  (setq-local corfu-auto t
              corfu-auto-delay 0.1
              corfu-auto-prefix 1)

  (if (bound-and-true-p lsp-mode)
      (setq-local completion-at-point-functions
                  (list #'lsp-completion-at-point
                        #'cape-dabbrev
                        #'cape-file))
    (setq-local completion-at-point-functions
                (list #'cape-dabbrev
                      #'cape-file)))

  (message "LSP and completion fixed! Start typing."))

(global-set-key (kbd "C-c C-l") 'my/fix-lsp-and-completion)

;;==============================================================================
;; INSTALLATION GUIDE
;;==============================================================================

;; Install language servers:
;;
;; Python:    pip install python-lsp-server[all]
;; Go:        go install golang.org/x/tools/gopls@latest
;; JS/TS:     npm install -g typescript typescript-language-server
;; C/C++:     sudo apt install clangd (or brew install llvm)
;; Rust:      rustup component add rust-analyzer

;;==============================================================================
;; KEYBINDINGS REFERENCE
;;==============================================================================

;; LSP (C-c L prefix):
;; C-c L a - Code actions
;; C-c L r - Rename
;; C-c L f - Format buffer
;; C-c L d - Describe at point
;; C-c L . - Peek definition
;; C-c L ? - Peek references
;; C-c L e - Error list
;; C-c L o - Organize imports

;; Completion (in popup):
;; TAB/S-TAB - Navigate
;; RET - Insert
;; M-d - Documentation
;; M-l - Location

;; Snippets:
;; C-c y - Expand snippet

;; Diagnostics:
;; C-c C-l - Fix LSP and completion


(provide 'languages)
;;; languages.el ends here
