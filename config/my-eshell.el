;;; my-eshell.el --- Enhanced eshell configuration for Emacs

;;; Commentary:
;; Comprehensive eshell enhancements including better prompt, completion,
;; visual improvements, aliases, history management, and git integration.

;;==============================================================================
;; BASIC ESHELL CONFIGURATION
;;==============================================================================

(require 'em-hist)
(require 'em-prompt)
(require 'em-cmpl)
(require 'em-alias)


;; Open eshell in current directory (enhanced version)
(defun my/eshell-here ()
  "Open eshell in current buffer's directory.
Reuses existing eshell buffer if available."
  (interactive)
  (let ((default-directory (my/current-dir)))
    (if (get-buffer "*eshell*")
        (progn
          (switch-to-buffer "*eshell*")
          (eshell t))
      (eshell))))

;; Alias for backward compatibility with existing keybindings
(defalias 'my/open-vterm-here 'my/eshell-here)

;;==============================================================================
;; COMPLETION ENHANCEMENTS
;;==============================================================================

;; Enable pcomplete for better command completion
(setq eshell-cmpl-ignore-case t
      eshell-cmpl-cycle-completions t
      eshell-cmpl-autolist t)

;; Tab completion like bash
(defun my/eshell-tab-complete ()
  "Tab completion for eshell.
If at beginning of line, insert tab.
Otherwise, call pcomplete to show completions and focus completion window."
  (interactive)
  (message "my/eshell-tab-complete called")
  
  (cond
   ((bolp)  ; Beginning of line - insert tab
    (insert "\t"))
    
   (t
    ;; Call pcomplete with error handling
    (condition-case err
        (progn
          (pcomplete)
          ;; Ensure display updates
          (redisplay t)
          ;; After showing completions, focus the completions window if it exists
          (let ((comp-buffer (get-buffer "*Completions*")))
            (when (and comp-buffer (get-buffer-window comp-buffer))
              (select-window (get-buffer-window comp-buffer)))))
      (error
       (message "pcomplete error: %s" (error-message-string err)))))))



;;==============================================================================
;; VISUAL IMPROVEMENTS
;;==============================================================================

;; Syntax highlighting for eshell buffers
;; Note: eshell-font-lock-keywords may not be defined in older Emacs versions
(when (boundp 'eshell-font-lock-keywords)
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local font-lock-defaults
                          '(eshell-font-lock-keywords t)))))

;;==============================================================================
;; ALIASES & SHORTCUTS
;;==============================================================================

;; Common Unix aliases
(setq eshell-command-aliases-list
      '(("ll" "ls -l")
        ("la" "ls -la")
        (".." "cd ..")
        ("..." "cd ../..")
        ("...." "cd ../../..")
        ("grep" "grep --color=auto")
        ("egrep" "egrep --color=auto")
        ("fgrep" "fgrep --color=auto")
        ("df" "df -h")
        ("du" "du -h")
        ("ps" "ps aux")
        ("cls" "clear")
        ("md" "mkdir -p")
        ("rd" "rmdir")
        ("h" "history")))


;; Quick directory navigation (like z/autojump)
(defun my/eshell-z (dir)
  "Quickly jump to a frequently used directory."
  (interactive "DDirectory: ")
  (eshell/cd dir))

;;==============================================================================
;; HISTORY MANAGEMENT
;;==============================================================================

;; Persistent history
(setq eshell-history-size 10000
      eshell-hist-ignoredups t
      eshell-save-history-on-exit t
      eshell-history-file-name (expand-file-name "~/.emacs.d/eshell/history"))

;; Create history directory if needed
(make-directory (file-name-directory eshell-history-file-name) t)

;; Better history search with consult
(defun my/eshell-history-search ()
  "Search eshell history with consult."
  (interactive)
  (when (fboundp 'consult-history)
    (consult-history eshell-history-ring)))



;; History completion
(setq eshell-cmpl-cycle-completions t
      eshell-cmpl-autolist t)

;;==============================================================================
;; GIT INTEGRATION
;;==============================================================================

;; Git status in prompt (already handled above)

;; Git commands in eshell
(defun my/eshell-git-status ()
  "Run git status in eshell."
  (interactive)
  (eshell-command "git status"))

(defun my/eshell-git-diff ()
  "Run git diff in eshell."
  (interactive)
  (eshell-command "git diff"))

;; Auto-detect git repos and show branch in prompt
(defun my/eshell-git-branch ()
  "Get current git branch for eshell prompt."
  (when (and (fboundp 'magit-get-current-branch)
             (locate-dominating-file default-directory ".git"))
    (magit-get-current-branch)))

;;==============================================================================
;; KEYBINDINGS
;;==============================================================================

;; Regular shell shortcuts (not Evil mode)
(with-eval-after-load 'eshell
  ;; Standard shell navigation
  (define-key eshell-mode-map (kbd "C-a") 'eshell-bol)
  (define-key eshell-mode-map (kbd "C-e") 'move-end-of-line)
  (define-key eshell-mode-map (kbd "C-d") 'eshell-delchar-or-maybe-eof)
  (define-key eshell-mode-map (kbd "C-k") 'eshell-kill-input)
  (define-key eshell-mode-map (kbd "C-u") 'eshell-kill-input)
  (define-key eshell-mode-map (kbd "C-w") 'backward-kill-word)
  (define-key eshell-mode-map (kbd "C-y") 'yank)
  ;; Character navigation
  (define-key eshell-mode-map (kbd "C-f") 'forward-char)
  (define-key eshell-mode-map (kbd "C-b") 'backward-char)
  ;; Word navigation
  (define-key eshell-mode-map (kbd "M-f") 'forward-word)
  (define-key eshell-mode-map (kbd "M-b") 'backward-word)
  ;; Tab completion
  (define-key eshell-mode-map (kbd "TAB") 'my/eshell-tab-complete)
  (define-key eshell-cmpl-mode-map (kbd "TAB") 'my/eshell-tab-complete)
  ;; Completion navigation (like bash)
  (when (fboundp 'pcomplete-expand)
    (define-key eshell-mode-map (kbd "C-n") 'pcomplete-expand)
    (define-key eshell-cmpl-mode-map (kbd "C-n") 'pcomplete-expand))
  (when (fboundp 'pcomplete-reverse)
    (define-key eshell-mode-map (kbd "C-p") 'pcomplete-reverse)
    (define-key eshell-cmpl-mode-map (kbd "C-p") 'pcomplete-reverse))
  ;; History search (like bash)
  (define-key eshell-mode-map (kbd "C-r") 'my/eshell-history-search)
  (define-key eshell-mode-map (kbd "C-s") 'isearch-forward))

;; Setup function for eshell
(defun my/eshell-setup ()
  "Configure eshell for regular shell shortcuts (not Evil mode)."
  (message "my-eshell: Setting up eshell buffer %s" (buffer-name))
  (evil-local-mode -1)
  (message "evil-local-mode after disable: %s" evil-local-mode)
  (when (fboundp 'evil-emacs-state)
    (evil-emacs-state))
  ;; Completion behavior
  (setq-local completion-cycle-threshold nil) ; Disable tab cycling - use window instead
  (setq-local completion-auto-help t)        ; Show completions window automatically
  (setq-local completion-auto-select t)      ; Auto-select completions window for navigation
  ;; Auto-focus completion window
  (setq-local display-buffer-alist
              (append (default-value 'display-buffer-alist)
                      '(("\\*Completions\\*"
                         (display-buffer-pop-up-window)
                         (window-height . 0.3)
                         (select . t)))))
  ;; Set keybindings locally (override any previous bindings)
  (define-key eshell-mode-map (kbd "C-a") 'eshell-bol)
  (define-key eshell-mode-map (kbd "C-e") 'move-end-of-line)
  (define-key eshell-mode-map (kbd "C-d") 'eshell-delchar-or-maybe-eof)
  (define-key eshell-mode-map (kbd "C-k") 'eshell-kill-input)
  (define-key eshell-mode-map (kbd "C-u") 'eshell-kill-input)
  (define-key eshell-mode-map (kbd "C-w") 'backward-kill-word)
  (define-key eshell-mode-map (kbd "C-y") 'yank)
  (define-key eshell-mode-map (kbd "C-f") 'forward-char)
  (define-key eshell-mode-map (kbd "C-b") 'backward-char)
  (define-key eshell-mode-map (kbd "M-f") 'forward-word)
  (define-key eshell-mode-map (kbd "M-b") 'backward-word)
  ;; Tab completion - set in multiple maps for maximum priority
  (define-key eshell-mode-map (kbd "TAB") 'my/eshell-tab-complete)
  (define-key eshell-cmpl-mode-map (kbd "TAB") 'my/eshell-tab-complete)
  (local-set-key (kbd "TAB") 'my/eshell-tab-complete)
  ;; Shift-Tab for reverse completion
  (define-key eshell-mode-map (kbd "S-TAB") 'pcomplete-reverse)
  (define-key eshell-cmpl-mode-map (kbd "S-TAB") 'pcomplete-reverse)
  (local-set-key (kbd "S-TAB") 'pcomplete-reverse)
  ;; Completion navigation (like bash)
  (when (fboundp 'pcomplete-expand)
    (define-key eshell-mode-map (kbd "C-n") 'pcomplete-expand)
    (define-key eshell-cmpl-mode-map (kbd "C-n") 'pcomplete-expand)
    (local-set-key (kbd "C-n") 'pcomplete-expand))
  (when (fboundp 'pcomplete-reverse)
    (define-key eshell-mode-map (kbd "C-p") 'pcomplete-reverse)
    (define-key eshell-cmpl-mode-map (kbd "C-p") 'pcomplete-reverse)
    (local-set-key (kbd "C-p") 'pcomplete-reverse))
  ;; Also remove any evil TAB bindings
  (when (boundp 'evil-emacs-state-local-map)
    (define-key evil-emacs-state-local-map (kbd "TAB") nil))
  (when (boundp 'evil-insert-state-local-map)
    (define-key evil-insert-state-local-map (kbd "TAB") nil))
  (define-key eshell-mode-map (kbd "C-r") 'my/eshell-history-search)
  (define-key eshell-mode-map (kbd "C-s") 'isearch-forward)
  (message "my-eshell: Shell shortcuts configured (TAB bound to %s)" (key-binding (kbd "TAB"))))

;; Add hook to run last (append)
(add-hook 'eshell-mode-hook #'my/eshell-setup -1)

;;==============================================================================
;; MISCELLANEOUS ENHANCEMENTS
;;==============================================================================

;; Directory tracking
(setq eshell-list-files-after-cd t)

;; Smart scrolling
(setq eshell-scroll-to-bottom-on-input 'all
      eshell-scroll-to-bottom-on-output 'all)

;; Output filtering
(add-hook 'eshell-output-filter-functions 'eshell-postoutput-scroll-to-bottom)

;;==============================================================================
;; COMPLETION LIST CONFIGURATION
;;==============================================================================

(defun my/eshell-completion-list-setup ()
  "Set up keybindings for completion list buffer."
  (when (derived-mode-p 'completion-list-mode)
    ;; Use C-n/C-p for navigation in addition to n/p
    (define-key completion-list-mode-map (kbd "C-n") 'next-completion)
    (define-key completion-list-mode-map (kbd "C-p") 'previous-completion)
    ;; Also allow Tab/Shift-Tab for navigation
    (define-key completion-list-mode-map (kbd "TAB") 'next-completion)
    (define-key completion-list-mode-map (kbd "S-TAB") 'previous-completion)
    (message "my-eshell: Completion list navigation configured")))

(add-hook 'completion-list-mode-hook #'my/eshell-completion-list-setup)

(provide 'my-eshell)
;;; my-eshell.el ends here
