;;; completion.el --- Completion framework (vertico, orderless, consult, embark) -*- lexical-binding: t -*-

;;; Commentary:
;; Completion framework configuration including vertico, orderless, consult,
;; embark, marginalia, and helpful.

;;==============================================================================
;; COMPLETION FRAMEWORK
;;==============================================================================

;; IDO-like behavior for file completion
(setq read-file-name-completion-ignore-case t
      completion-ignore-case t
      completion-flex-nospace t
      completion-pcm-word-delimiters "-_./| "
      completion-pcm-complete-word-inserts-delimiters t)

;;; Vertico - vertical completion UI
(use-package vertico
  :ensure t
  :init
  (vertico-mode 1)
  :config
  (setq vertico-cycle t  ; Cycle through candidates
        vertico-count 10) ; Show 10 lines by default
  ;; Tab completion in vertico
  (define-key vertico-map (kbd "TAB") #'minibuffer-complete))

(use-package vertico-multiform
  :ensure nil
  :after vertico
  :config
  (when (fboundp 'vertico-multiform-mode)
    (vertico-multiform-mode)
    (setq vertico-multiform-categories
          '((consult-grep (vertico-grid . (:columns 2)))))
    (setq vertico-grid-separator "    ")))

;;; Orderless - flexible matching (fuzzy search)
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles orderless partial-completion basic))))
  (setq orderless-matching-styles '(orderless-flex
                                    orderless-regexp
                                    orderless-literal)
        orderless-component-separator "[ &]"
        orderless-style-dispatchers nil))

;;; Marginalia - rich annotations in minibuffer
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode 1))

;;; Consult - enhanced commands with preview
(use-package consult
  :ensure t
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
          ("C-x M-:" . consult-complex-command)
           ("C-x C-f" . find-file)
          ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r b" . consult-bookmark)
         ;; M-g bindings (goto-map)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
           ;; M-s bindings (search-map)
           ("M-s f" . my/consult-find-current-dir)
           ("M-s d" . my/find-file-fd)
           ("M-s r" . recentf-open-files)
           ("M-s /" . my/consult-ripgrep-current-dir)
          ;; Other custom bindings
          ("M-y" . consult-yank-pop))
   :config
   ;; Preview key
   (setq consult-preview-key 'any
         consult-preview-max-size 1.0400
         consult-preview-raw-size 307200)
   ;; Narrow key
   (setq consult-narrow-key "<")
   ;; Configure consult-ripgrep for better performance
   (setq consult-ripgrep-args
         '("rg" "--null" "--line-buffered" "--color=never" "--max-columns=1000"
           "--path-separator" "/" "--smart-case" "--no-heading" "--with-filename"
           "--line-number" "--search-zip" "--hidden" "-g" "!.git/"))
   ;; Configure consult-find for recursive file search
   (setq consult-find-args
         '("find" "." "-type" "f" "(" "-path" "*/.git/*" "-o" "-path" "*/.svn/*" "-o" "-path" "*/.hg/*" ")" "-prune" "-o" "-type" "f" "-print"))
    ;; Configure consult-fd for faster recursive file search
    (setq consult-fd-args '("fd" "--type" "f" "--hidden" "--exclude" ".git"))

     (define-key vertico-map (kbd "C-l") #'consult-preview-atpoint))



;;; Find file with fd (fast recursive search)
(defun my/find-file-fd (&optional dir)
  "Find file recursively using fd (faster alternative to find).
When called with prefix argument, prompt for starting directory.
Otherwise start from home directory (~/).
If fd command is not available, falls back to consult-find."
  (interactive "P")
  (let ((root-dir (if dir
                      (read-directory-name "Search from directory: ")
                    (expand-file-name "~"))))
    (if (executable-find "fd")
        (progn
          (message "Using fd for file search in %s" root-dir)
          (let ((default-directory root-dir))
            (consult-fd "")))
      (message "fd command not found. Using consult-find instead.")
      (consult-find root-dir))))



;;; Embark - contextual actions
(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
   :config
   (setq embark-quit-after-action nil)
   ;; Hide the mode line of the Embark live/completions buffers
   (add-to-list 'display-buffer-alist
                '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                  nil
                  (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;; Enable savehist-mode for command history
(use-package savehist
  :ensure t
  :init
  (savehist-mode 1))

;; Keep helpful since it's referenced in keybindings.el
(use-package helpful
  :ensure t
  :defer t
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

;;==============================================================================
;; GLOBAL COMPLETION SETTINGS
;;==============================================================================

;; Show completion list when multiple options exist
(setq completion-auto-help t
      completion-auto-select t
      completion-cycle-threshold t)

(provide 'completion)
;;; completion.el ends here