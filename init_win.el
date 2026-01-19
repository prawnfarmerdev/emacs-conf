;;; init.el --- Emacs Configuration with Tmux/Vim-style keybindings (Windows) -*- lexical-binding: t -*-

;;; Commentary:
;; Tmux-style buffer management with Vim keybindings
;; C-f: Fuzzy find directories (like fzf)
;; C-SPC /: Search text in current directory (toggle regex/fixed with C-SPC g)
;; Windows-compatible version with proper path handling

;;; Code:

;;==============================================================================
;; WINDOWS-SPECIFIC SETTINGS
;;==============================================================================

;; Set Windows-friendly paths
(when (eq system-type 'windows-nt)
  (setq w32-pipe-read-delay 0)
  (setq w32-pipe-buffer-size (* 64 1024))
  
  ;; Use UTF-8 everywhere
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  
  ;; Performance tweaks for Windows
  (setq w32-get-true-file-attributes nil)
  (setq inhibit-compacting-font-caches t))

;;==============================================================================
;; STARTUP OPTIMIZATIONS
;;==============================================================================

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1)))

(setq-default inhibit-redisplay t
              inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redisplay)))

(setq read-process-output-max (* 1024 1024)
      process-adaptive-read-buffering nil
      inhibit-compacting-font-caches t)

(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right
              bidi-inhibit-bpa t)

(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq native-comp-async-report-warnings-errors nil
        native-comp-deferred-compilation t))

;;==============================================================================
;; PACKAGE MANAGEMENT
;;==============================================================================

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; EVIL MODE - VIM KEYBINDINGS
;;==============================================================================

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-undo-system 'undo-redo)
  :config
  (evil-mode 1)
  
  ;; Ctrl-c exits to normal mode
  (define-key evil-insert-state-map (kbd "C-c") 'evil-normal-state)
  (define-key evil-visual-state-map (kbd "C-c") 'evil-normal-state)
  (define-key evil-replace-state-map (kbd "C-c") 'evil-normal-state)
  
  ;; Ctrl-Shift-c for copy
  (define-key evil-visual-state-map (kbd "C-S-c") 'evil-yank)
  (define-key evil-normal-state-map (kbd "C-S-c") 'evil-yank-line)
  
  ;; Unbind 't' in normal mode to avoid conflict with terminal prefix
  (define-key evil-normal-state-map (kbd "t") nil)
  
  (setq-default indicate-empty-lines t
                indicate-buffer-boundaries nil))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (setq evil-collection-mode-list 
        (delq 'ibuffer evil-collection-mode-list))
  (evil-collection-init))

;;==============================================================================
;; WHICH-KEY
;;==============================================================================

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3
        which-key-idle-secondary-delay 0.0))

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

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)
         ("M-g g" . consult-goto-line)
         ("M-g i" . consult-imenu))
  :config
  (define-key vertico-map (kbd "C-l") #'consult-preview-at-point)
  
  ;; Configure consult-ripgrep for Windows (if ripgrep is installed)
  (setq consult-ripgrep-args
        '("rg" "--null" "--line-buffered" "--color=never" "--max-columns=1000"
          "--path-separator" "/" "--smart-case" "--no-heading" "--line-number"
          "--hidden" "-g" "!.git/" "--" ".")))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package helpful
  :ensure t
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

;;==============================================================================
;; CONSULT DIRECTORY FINDER (Windows)
;;==============================================================================

(defun my/consult-project-dirs-windows ()
  "Consult-based directory selection for Windows."
  (interactive)
  (let* ((dirs (list (expand-file-name "~/Documents")
                     (expand-file-name "~/Desktop")
                     (expand-file-name "~/projects")
                     (expand-file-name "~")
                     "C:/projects"
                     "D:/projects"))
         (candidates (mapcan (lambda (dir)
                               (when (file-directory-p dir)
                                 (condition-case err
                                     (mapcar (lambda (file)
                                               (when (and (file-directory-p file)
                                                          (not (member (file-name-nondirectory file) '("." ".."))))
                                                 file))
                                             (directory-files dir t nil t))
                                   (error
                                    (message "Cannot read directory %s: %s" dir (error-message-string err))
                                    nil))))
                             dirs))
         (filtered-candidates (delq nil candidates)))
    (when filtered-candidates
      (consult--read filtered-candidates
                     :prompt "Select directory: "
                     :require-match t
                     :sort nil
                     :category 'file
                     :history 'my/directory-history
                     :action (lambda (selected)
                               (when selected
                                 (dired selected)))))))

;;==============================================================================
;; VTERM (Windows alternative - use eshell or powershell)
;;==============================================================================

;; On Windows, vterm doesn't work well, so we use eshell or powershell instead


;; PowerShell integration for better terminal support on Windows
(use-package powershell
  :ensure t
  :if (eq system-type 'windows-nt)
  :config
  ;; Configure powershell.el for better terminal experience
  (setq powershell-args '("-NoExit" "-NoLogo" "-NoProfile"))
  ;; Ensure powershell buffers are properly configured for SSH
  (add-hook 'powershell-mode-hook
            (lambda ()
              (setq-local comint-process-echoes nil)
              (setq-local comint-use-prompt-regexp t)
              (setq-local comint-prompt-regexp "^PS.*> "))))

;; Eat terminal emulator for excellent Windows and SSH support
(use-package eat
  :ensure t
  :config
  ;; Configure eat for Windows SSH compatibility
  (defun my/configure-eat-mode-windows ()
    "Configure eat-mode for Windows SSH compatibility."
    (when (derived-mode-p 'eat-mode)
      (when (eq system-type 'windows-nt)
        ;; Eat provides excellent terminal emulation, SSH should work well
        (setq-local eat-term-prompt-regexp "^[A-Z]:\\.*?> \\|^PS.*> ")
        ;; Eat handles pseudo-terminal allocation better than shell-mode
        (message "eat-mode: SSH should work without -t flag on Windows"))))
  (add-hook 'eat-mode-hook #'my/configure-eat-mode-windows)
  
  ;; Windows workaround for eat's hardcoded /usr/bin/env sh -c command
  (when (eq system-type 'windows-nt)
    ;; Define advice function
    (defun my/eat-exec-windows-advice (orig-fun buffer name command startfile switches)
      "Adjust eat-exec arguments for Windows compatibility."
      (if (string= command "/usr/bin/env")
          ;; Replace Unix-style command with Windows shell
          (let* ((shell (if (executable-find "powershell.exe")
                            "powershell.exe"
                          "cmd.exe"))
                 (args (if (executable-find "powershell.exe")
                           '("-NoExit" "-NoLogo" "-NoProfile")
                         '("/k"))))
            (funcall orig-fun buffer name shell startfile args))
        ;; Otherwise call original
        (funcall orig-fun buffer name command startfile switches)))
    ;; Add advice only once
    (unless (advice-member-p #'my/eat-exec-windows-advice 'eat-exec)
      (advice-add 'eat-exec :around #'my/eat-exec-windows-advice))))

;;==============================================================================
;; HELPER FUNCTIONS
;;==============================================================================

(defvar consult-ripgrep-args
  '("rg" "--null" "--line-buffered" "--color=never" "--max-columns=1000"
    "--path-separator" "/" "--smart-case" "--no-heading" "--line-number"
    "--hidden" "-g" "!.git/" "--" ".")
  "Command line arguments for consult-ripgrep.")

(defun my/current-dir ()
  "Return directory of current buffer, or `default-directory`."
  (if-let ((file (buffer-file-name)))
      (file-name-directory file)
    default-directory))

(defun my/open-current-dir-dired ()
  "Open dired in current buffer's directory."
  (interactive)
  (dired (my/current-dir)))

(defun my/open-eshell-here ()
  "Open eshell in current buffer's directory."
  (interactive)
  (let ((default-directory (my/current-dir)))
    (eshell t)))

(defun my/open-shell-here ()
  "Open shell in current buffer's directory with PowerShell detection."
  (interactive)
  (let ((default-directory (my/current-dir)))
    (cond
     ;; Use powershell.el if available on Windows
     ((eq system-type 'windows-nt)
      (condition-case err
          (progn
            (unless (fboundp 'powershell)
              (require 'powershell))
            (powershell))
        (error
         ;; Fall back to cmd.exe
         (let ((explicit-shell-file-name "cmd.exe")
               (explicit-shell-args '("/k")))
           (shell)))))
     ;; Unix/Linux: try pwsh then bash
     ((executable-find "pwsh")
      (let ((explicit-shell-file-name "pwsh")
            (explicit-shell-args '("-NoExit" "-NoLogo" "-NoProfile" "-Command" "-")))
        (shell)))
       (t
        (let ((explicit-shell-file-name "/bin/bash"))
          (shell))))))

(defun my/open-eat-here ()
  "Open eat (Emulated Advanced Terminal) in current buffer's directory.
Eat is a terminal emulator with excellent Windows and SSH support.
On Windows, provides proper shell arguments to avoid spawn errors."
  (interactive)
  (let ((default-directory (my/current-dir))
        (shell (cond
                ((eq system-type 'windows-nt)
                 (if (executable-find "powershell.exe")
                     "powershell.exe"
                   "cmd.exe"))
                (t
                 (or (executable-find "bash") "/bin/bash"))))
        (shell-args (cond
                     ((eq system-type 'windows-nt)
                      (if (executable-find "powershell.exe")
                          '("-NoExit" "-NoLogo" "-NoProfile")
                        '("/k")))
                     (t
                      nil))))
    (condition-case err
        (progn
          ;; Ensure eat is loaded
          (require 'eat nil t)
          (if shell-args
              ;; Use eat-make to pass shell arguments
              (eat-make (generate-new-buffer-name "*eat*") shell nil shell-args)
            ;; No arguments, use plain eat
            (eat shell)))
      (error
       (message "eat failed: %s, falling back to shell" (error-message-string err))
       (my/open-shell-here)))))

;; Configure shell mode for SSH compatibility
(defun my/configure-shell-mode-windows ()
  "Configure shell mode for Windows SSH compatibility."
  (when (derived-mode-p 'shell-mode)
    (when (stringp explicit-shell-file-name)
      (let ((shell-name (file-name-nondirectory explicit-shell-file-name)))
        (cond
         ((string-match "powershell\\.exe\\|pwsh" shell-name)
          ;; PowerShell settings
          (setq-local comint-process-echoes nil)
          (setq-local comint-use-prompt-regexp t)
          (setq-local comint-prompt-regexp "^PS.*> "))
         ((string-match "cmd\\.exe" shell-name)
          ;; cmd.exe settings for SSH
          (setq-local comint-process-echoes t)
          (setq-local comint-use-prompt-regexp t)
          (setq-local comint-prompt-regexp "^[A-Z]:\\.*?> ")
          ;; Note: cmd.exe doesn't support SSH aliases, so SSH needs -t flag
          (message "For SSH on Windows, use 'ssh -t hostname' or eshell with ssh alias"))
         (t
          ;; Unix shell defaults
          (setq-local comint-process-echoes nil)
          (setq-local comint-use-prompt-regexp nil)))))))

;; Add shell configuration hook
(add-hook 'shell-mode-hook #'my/configure-shell-mode-windows)



(defvar my/search-use-regexp t
  "If non-nil, use regexp search with consult-ripgrep.
If nil, use fixed strings search.")

(defun my/consult-ripgrep-toggle-regex ()
  "Toggle between regexp and fixed string search for consult-ripgrep."
  (interactive)
  (setq my/search-use-regexp (not my/search-use-regexp))
  (message "Search mode: %s" (if my/search-use-regexp "regexp" "fixed strings")))

(defun my/consult-ripgrep-current-dir ()
  "Run consult-ripgrep in current directory.
Uses regexp or fixed strings based on `my/search-use-regexp`.
Falls back to consult-grep if ripgrep is not available."
  (interactive)
  (if (executable-find "rg")
      (let ((args (if (boundp 'consult-ripgrep-args)
                      (if my/search-use-regexp
                          consult-ripgrep-args
                        (cons "--fixed-strings" consult-ripgrep-args))
                    '("rg" "--null" "--line-buffered" "--color=never" "--max-columns=1000"
                      "--path-separator" "/" "--smart-case" "--no-heading" "--line-number"
                      "--hidden" "-g" "!.git/" "--" "."))))
        (let ((consult-ripgrep-args args))
          (consult-ripgrep (my/current-dir))))
    (message "Ripgrep not found, using consult-grep instead")
    (consult-grep (my/current-dir))))

(defun my/consult-find-current-dir ()
  "Run consult-find in current directory for fuzzy file search."
  (interactive)
  (consult-find (my/current-dir)))

;;==============================================================================
;; WINDOWS SSH CONFIGURATION
;;==============================================================================

;; Configure eshell for Windows SSH compatibility
(defun my/setup-windows-eshell-ssh ()
  "Set up eshell aliases for Windows SSH compatibility."
  (when (eq system-type 'windows-nt)
    ;; Add ssh alias that forces pseudo-terminal allocation
    (setq eshell-command-aliases-list
          (append eshell-command-aliases-list
                  '(("ssh" "ssh -t $*")
                    ("scp" "scp $*")
                    ("sftp" "sftp $*"))))))

;; Run eshell SSH setup when eshell loads
(with-eval-after-load 'eshell
  (add-hook 'eshell-mode-hook #'my/setup-windows-eshell-ssh))

;; Interactive SSH function for Windows
(defun my/ssh-windows (host)
  "SSH to HOST with proper pseudo-terminal allocation for Windows."
  (interactive "sSSH to host: ")
  (let ((default-directory (my/current-dir)))
    (my/open-shell-here)
    (comint-send-string (get-buffer-process (current-buffer)) 
                        (format "ssh -t %s\n" host))))

;;==============================================================================
;; TMUX-STYLE KEYBINDINGS
;;==============================================================================

(use-package general
  :ensure t
  :demand t
  :config
  
  ;; Unbind C-SPC from set-mark-command to use as leader
  (general-unbind "C-SPC")
  (general-create-definer my-leader-def
    :states '(normal visual)
    :keymaps 'global
    :prefix "C-SPC")
  
  (my-leader-def
    ;; Files
    "f"  '(:ignore t :which-key "files")
    "ff" '(find-file :which-key "find file")
    "fg" '(my/consult-find-current-dir :which-key "fuzzy find files")
    "fs" '(save-buffer :which-key "save file")
    "fr" '(recentf-open-files :which-key "recent files")
    "fd" '(my/consult-project-dirs-windows :which-key "find directory (consult)")
    
    ;; Search
    "/" '(my/consult-ripgrep-current-dir :which-key "search text in current dir")
    "g" '(my/consult-ripgrep-toggle-regex :which-key "toggle regex/fixed search")
    
    ;; Buffer single keys
    "s" '(ibuffer :which-key "list buffers")
    "x" '(kill-buffer :which-key "kill buffer")
    "c" '(find-file :which-key "new buffer/find file")
    "t"  '(:ignore t :which-key "terminal")
    "te" '(my/open-eshell-here :which-key "eshell")
    "tt" '(my/open-eat-here :which-key "eat")
    ;; Buffer navigation
    "b"  '(:ignore t :which-key "buffer")
    "bn" '(next-buffer :which-key "next buffer")
    "bp" '(previous-buffer :which-key "previous buffer")
    "bl" '(evil-switch-to-windows-last-buffer :which-key "last buffer")
    
    ;; Windows
    "w"  '(:ignore t :which-key "windows")
    "ws" '(split-window-below :which-key "split horizontal")
    "wv" '(split-window-right :which-key "split vertical")
    "wd" '(delete-window :which-key "delete window")
    "wo" '(delete-other-windows :which-key "only this window")
    "wh" '(windmove-left  :which-key "left")
    "wj" '(windmove-down  :which-key "down")
    "wk" '(windmove-up    :which-key "up")
    "wl" '(windmove-right :which-key "right")
    "ww" '(other-window   :which-key "next window")
    ;; Tmux-style alternative bindings
    "w\"" '(split-window-below :which-key "split horizontal (tmux)")
    "w%" '(split-window-right :which-key "split vertical (tmux)")
    "wq" '(delete-window :which-key "close pane (tmux)")
    "wz" '(delete-other-windows :which-key "zoom pane (tmux)")
    
    ;; Help
    "h"  '(:ignore t :which-key "help")
    "hf" '(helpful-callable :which-key "describe function")
    "hv" '(helpful-variable :which-key "describe variable")
    "hk" '(helpful-key :which-key "describe key")
    
    ;; Quit
    "q"  '(:ignore t :which-key "quit")
    "qq" '(save-buffers-kill-terminal :which-key "quit emacs"))
  
  ;; Numbered buffer jumping (C-SPC 1-9)
  (dotimes (i 9)
    (let ((n (1+ i)))
      (general-define-key
       :states '(normal insert visual emacs)
       :keymaps 'global
       (concat "C-SPC " (number-to-string n))
       `(lambda ()
          (interactive)
          (let ((buf (nth ,(1- n) (buffer-list))))
            (when buf (switch-to-buffer buf)))))))
  
  ;; Font size controls (work in all modes)
  (general-define-key
   :states '(normal insert visual emacs)
   :keymaps 'global
   "C-=" 'text-scale-increase
   "C--" 'text-scale-decrease
   "C-0" 'text-scale-adjust)
  
  ;; C-f for FZF directory search (global)
  (general-define-key
   :states '(normal insert visual emacs)
   :keymaps 'global
    "C-f" 'my/consult-project-dirs-windows))

;; Quick access bindings in normal mode (no prefix)
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "-") #'my/open-current-dir-dired))

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
  (set-face-attribute 'hl-line nil :background "#1a1a1a")
  
  ;; Yellow cursor
  (set-face-attribute 'cursor nil :background "#b58900")
  
  ;; Line numbers
  (set-face-attribute 'line-number nil 
                      :background "#000000" 
                      :foreground "#b58900"
                      :height 1.0)
  (set-face-attribute 'line-number-current-line nil 
                      :background "#000000" 
                      :foreground "#d33682"
                      :height 1.0
                      :weight 'bold)
  
  ;; Mode line
  (set-face-attribute 'mode-line nil 
                      :background "#268bd2"
                      :foreground "#fdf6e3"
                      :height 1.0)
  (set-face-attribute 'mode-line-inactive nil 
                      :background "#073642"
                      :foreground "#586e75"
                      :height 1.0)
  
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
;; FONT CONFIGURATION
;;==============================================================================

(defvar my/default-font-size 150)

;; Try JetBrainsMono, fallback to Consolas (Windows default)
(let ((font (cond
             ((member "JetBrainsMono Nerd Font" (font-family-list))
              "JetBrainsMono Nerd Font")
             ((member "Consolas" (font-family-list))
              "Consolas")
             (t "Courier New"))))
  (set-face-attribute 'default nil
                      :family font
                      :weight 'bold
                      :height my/default-font-size)
  
  (set-face-attribute 'fixed-pitch nil
                      :family font
                      :weight 'bold
                      :height my/default-font-size))

(pixel-scroll-precision-mode 1)

;;==============================================================================
;; UI SETTINGS
;;==============================================================================

(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

(setq idle-update-delay 1.0
      cursor-in-non-selected-windows nil
      highlight-nonselected-windows nil)

;;==============================================================================
;; EDITOR BEHAVIOR
;;==============================================================================

(setq-default indent-tabs-mode nil
              tab-width 4
              truncate-lines t)

(setq truncate-partial-width-windows nil)

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(save-place-mode 1)
(recentf-mode 1)
(setq recentf-max-saved-items 100)

(show-paren-mode 1)
(global-hl-line-mode 1)

(setq show-paren-delay 0
      scroll-margin 8
      scroll-conservatively 101
      scroll-preserve-screen-position t
      fast-but-imprecise-scrolling t
      redisplay-skip-fontification-on-input t)

(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist default-file-name-handler-alist)))

;; Windows-friendly backup directory
(setq backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      auto-save-default t
      auto-save-timeout 20
      auto-save-interval 200)

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
;; WINDOWS SSH NOTES
;;==============================================================================

;; For SSH to work properly on Windows:
;; 1. Install OpenSSH Client (Windows 10/11):
;;    - Settings > Apps > Optional Features > Add Feature > OpenSSH Client
;;    - Or PowerShell: `Add-WindowsCapability -Online -Name OpenSSH.Client~~~~0.0.1.0`
;; 2. SSH requires pseudo-terminal allocation on Windows:
;;    - In eshell: Use `ssh host` (automatically adds -t flag via alias)
;;    - In shell mode: Use `ssh -t host` (manual -t flag required)
;;    - Or use `M-x my/ssh-windows` interactive function
;; 3. For best results, use eat terminal (C-SPC t t) for excellent SSH support
;; 4. Alternative: powershell.el terminal or eshell with auto -t flag
;; 5. Consider using TRAMP for remote file editing instead of SSH shell

;;; init.el ends here
