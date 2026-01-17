;;; completion.el --- Completion framework (vertico, orderless, consult, embark)

;;; Commentary:
;; Completion framework configuration including vertico, orderless, consult,
;; embark, marginalia, and helpful.

;;==============================================================================
;; COMPLETION FRAMEWORK
;;==============================================================================

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :config
  (set-face-attribute 'minibuffer-prompt nil :height 1.0)
  (add-hook 'minibuffer-setup-hook
            (lambda ()
              (setq-local face-remapping-alist
                          '((default :height 1.0))))))

(use-package vertico-multiform
  :defer t
  :after vertico
  :config
  (vertico-multiform-mode)
  (setq vertico-multiform-categories
        '((consult-grep (vertico-grid . (:columns 2)))))
  (setq vertico-grid-separator "    "))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  :config
  (setq orderless-matching-styles '(orderless-regexp)
        orderless-component-separator " *"
        orderless-style-dispatchers nil))

(use-package consult
  :ensure t
  :defer t
  :bind (("C-x b" . consult-buffer)
         ("M-g g" . consult-goto-line)
         ("M-g i" . consult-imenu))
  :init
    ;; Configure consult-ripgrep for better performance (set early for helpers.el)
    (setq consult-ripgrep-args
          '("rg" "--null" "--line-buffered" "--color=never" "--max-columns=1000"
            "--path-separator" "/" "--smart-case" "--no-heading" "--line-number"
             "--hidden" "-g" "!.git/" "--" "."))
  :config
   (define-key vertico-map (kbd "C-l") #'consult-preview-atpoint)
    
    ;; Enable preview for all consult commands
    (setq consult-preview-key 'any
          consult-preview-max-size 1.0400
          consult-preview-raw-size 307200))

(use-package embark
 :ensure t
 :defer t
 :bind
 (("C-." . embark-act)
  ("C-," . embark-dwim))
 :config
 (setq embark-quit-after-action nil)
 (add-to-list 'display-buffer-alist
              '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                nil (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :defer t
  :after (embark consult)
  :config
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

(use-package marginalia
  :ensure t
  :defer t
  :config
  (marginalia-mode))

(use-package helpful
  :ensure t
  :defer t
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(provide 'completion)
;;; completion.el ends here