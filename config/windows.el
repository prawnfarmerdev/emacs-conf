;;; windows.el --- Windows-specific configuration overrides -*- lexical-binding: t -*-

;;; Commentary:
;; Windows-specific configuration overrides loaded conditionally when
;; `my/is-windows` is true.

;;==============================================================================
;; WINDOWS PERFORMANCE & SYSTEM TWEAKS
;;==============================================================================

;; Windows-specific performance optimizations
(setq w32-pipe-read-delay 0)
(setq w32-pipe-buffer-size (* 64 1024))
(setq w32-get-true-file-attributes nil)
(setq inhibit-compacting-font-caches t)

;; Use UTF-8 everywhere
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Display optimization for Windows
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right
              bidi-inhibit-bpa t)

;; Native compilation settings for Windows
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq native-comp-async-report-warnings-errors nil
        native-comp-deferred-compilation t))

;;==============================================================================
;; WINDOWS PROCESS & I/O OPTIMIZATIONS
;;==============================================================================

;; Process output buffering for better performance
(setq read-process-output-max (* 1024 1024))
(setq process-adaptive-read-buffering nil)

;; Startup display optimizations
(setq-default inhibit-redisplay t
              inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redisplay)))

;;==============================================================================
;; WINDOWS FONT CONFIGURATION
;;==============================================================================

;; Font configuration for Windows with Nerd Font support
(defvar my/windows-nerd-font-priority
  '("Cascadia Code NF"  ; Cascadia Code with Nerd Font glyphs
    "CaskaydiaCove NF"   ; CaskaydiaCove with Nerd Font glyphs  
    "JetBrainsMono Nerd Font"
    "Fira Code Retina"
    "Consolas"
    "Courier New")
  "Priority list of fonts for Windows, nerd fonts first.")

(defun my/setup-windows-fonts ()
  "Configure fonts for Windows with nerd font support."
  (let ((font-found nil)
        (font-name nil))
    ;; Try to find an available nerd font
    (dolist (font my/windows-nerd-font-priority)
      (when (and (not font-found) (member font (font-family-list)))
        (setq font-found t)
        (setq font-name font)))
    
    ;; If no font found, use default
    (unless font-found
      (setq font-name "Consolas"))
    
    ;; Set the font
    (setq my/default-font
          (font-spec :family font-name
                     :weight 'bold
                     :size 12))
    
    (message "Windows font set to: %s" font-name)
    font-name))

;; Run font setup
(my/setup-windows-fonts)
;;==============================================================================
;; WINDOWS TERMINAL CONFIGURATION
;;==============================================================================

;; Use eshell as default terminal on Windows
(defun my/open-eshell-here ()
  "Open terminal (eshell) in current directory (Windows version)."
  (interactive)
  (let ((default-directory (if (bound-and-true-p my/current-dir)
                               (funcall 'my/current-dir)
                             default-directory)))
    (eshell t)))

;; Windows shell configuration for better SSH compatibility
(defun my/open-shell-here-windows ()
  "Open shell on Windows with better terminal support for SSH.
Uses powershell.el if available, otherwise configures cmd.exe for SSH."
  (interactive)
  (let ((default-directory (if (bound-and-true-p my/current-dir)
                               (funcall 'my/current-dir)
                             default-directory)))
    (condition-case err
        (progn
          ;; Try to use powershell.el for better terminal integration
          (unless (fboundp 'powershell)
            (require 'powershell))
          (powershell))
      (error
       ;; Fall back to cmd.exe with SSH-friendly configuration
       (message "powershell.el failed: %s, using cmd.exe" (error-message-string err))
       (let ((explicit-shell-file-name "cmd.exe")
             (explicit-shell-args '("/k")))
         ;; Configure comint for better terminal emulation
         (add-hook 'shell-mode-hook 
                   (lambda ()
                     (when (string-match "cmd\\.exe" (or explicit-shell-file-name ""))
                       (setq-local comint-process-echoes t)
                       (setq-local comint-use-prompt-regexp t)
                       (setq-local comint-prompt-regexp "^[A-Z]:\\.*?> "))))
         (shell))))))

;; SSH alias for Windows to force pseudo-terminal allocation
(defun my/ssh-with-pty (host)
  "SSH to HOST with forced pseudo-terminal allocation for Windows."
  (interactive "sSSH to host: ")
  (let ((cmd (format "ssh -t %s" host)))
    (if (eq system-type 'windows-nt)
        (my/open-shell-here-windows)
      (shell))
    (comint-send-string (get-buffer-process (current-buffer)) cmd)
    (comint-send-input)))

;; Configure shell aliases for Windows SSH compatibility
(defun my/setup-windows-shell-aliases ()
  "Set up shell aliases for better SSH experience on Windows."
  ;; For shell mode (cmd.exe or powershell)
  (when (eq system-type 'windows-nt)
    ;; Add ssh alias that forces pseudo-terminal
    (setq shell-command-switch "-c")
    ;; Note: cmd.exe doesn't support aliases like bash, so we'd need a wrapper function
    ;; Instead, we'll configure eshell aliases
    ))
  
;; Configure eshell aliases for Windows
(defun my/setup-windows-eshell-aliases ()
  "Set up eshell aliases for Windows SSH compatibility."
  (when (eq system-type 'windows-nt)
    (setq eshell-command-aliases-list
          (append eshell-command-aliases-list
                  '(("ssh" "ssh -t $*")
                    ("scp" "scp $*")
                    ("sftp" "sftp $*"))))))

;; Run setup when eshell loads
(with-eval-after-load 'eshell
  (add-hook 'eshell-mode-hook #'my/setup-windows-eshell-aliases))

;;==============================================================================
;; WINDOWS-SPECIFIC PATHS & BACKUPS
;;==============================================================================

;; Windows-friendly backup directory
(setq backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory))))

;; Override directory candidates for Windows
(defun my/get-directory-candidates ()
  "Return list of subdirectory paths from common Windows directories."
  (let ((roots (list (expand-file-name "~/Documents")
                     (expand-file-name "~/Projects")
                     (expand-file-name "~")
                     (expand-file-name "~/Desktop")
                     (expand-file-name "C:/Users")
                     (expand-file-name "C:/Projects")))
        candidates)
    (dolist (root roots candidates)
      (when (file-directory-p root)
        (condition-case err
            (dolist (file (directory-files root t nil t))  ; full paths, no match, nosort
              (let ((full-path file))
                (when (and (file-directory-p full-path)
                           (not (member (file-name-nondirectory full-path) '("." ".."))))
                  (push full-path candidates))))
          (error
           (message "Cannot read directory %s: %s" root (error-message-string err))))))))

;;==============================================================================
;; EXECUTABLE OVERRIDES FOR WINDOWS
;;==============================================================================



;; Override consult-ripgrep args for Windows
(setq consult-ripgrep-args
      '("rg.exe" "--null" "--line-buffered" "--color=never" "--max-columns=1000"
        "--path-separator" "\\" "--smart-case" "--no-heading" "--with-filename"
        "--line-number" "--search-zip" "--hidden" "-g" "!.git/"))



;;==============================================================================
;; POWERSHELL INTEGRATION
;;==============================================================================

(use-package powershell
  :ensure t
  :defer t
  :if (eq system-type 'windows-nt))

;;==============================================================================
;; INSTALLATION NOTES
;;==============================================================================

;; Note: Windows users should install:
 ;; 1. ripgrep for Windows: https://github.com/BurntSushi/ripgrep/releases
 ;; 2. OpenSSH Client (for SSH functionality):
 ;;    - Windows 10/11: Settings > Apps > Optional Features > Add Feature > OpenSSH Client
 ;;    - Or via PowerShell: `Add-WindowsCapability -Online -Name OpenSSH.Client~~~~0.0.1.0`
 ;; 3. For best SSH experience, use eat terminal (C-SPC t t):
 ;;    - Provides proper pseudo-terminal emulation for SSH
 ;;    - SSH works without -t flag in eat terminal
 ;;    - Eat is auto-installed via package configuration
 ;; 4. Alternative: eshell (C-SPC t e) auto-adds -t flag for SSH
 ;; 5. Configure SSH to use -t flag for pseudo-terminal allocation (auto-configured in eshell)

 ;; PowerShell is already included in Windows 10+

(provide 'windows)
;;; windows.el ends here