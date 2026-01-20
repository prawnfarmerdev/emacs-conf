;;; keybindings.el --- Keybindings for terminal testing -*- lexical-binding: t -*-

;;; Commentary:
;; Simple keybindings for testing terminal options
;; Works with or without general package

;;==============================================================================
;; GLOBAL KEYBINDINGS
;;==============================================================================

;; Function keys for quick testing
(global-set-key (kbd "<f1>") 'my/terminal-quick-help)
(global-set-key (kbd "<f5>") 'my/compare-terminals)
(global-set-key (kbd "<f6>") 'my/test-ssh-mfa)
(global-set-key (kbd "<f7>") 'my/debug-terminal-setup)
(global-set-key (kbd "<f8>") 'my/fix-child-process-error)
(global-set-key (kbd "<f9>") 'my/test-all-ssh-methods)

;; Quick access to terminals
(global-set-key (kbd "C-'") 'my/terminal-eshell)
(global-set-key (kbd "C-\"") 'my/ssh-tramp-mfa)

;; Leader key emulation (C-SPC prefix)
(global-unset-key (kbd "C-SPC"))
(defvar my/leader-map (make-sparse-keymap)
  "Keymap for leader key bindings.")

(defun my/leader-prefix ()
  "Activate leader keymap."
  (interactive)
  (let ((keys (read-key-sequence "Leader: ")))
    (if (keymapp (lookup-key my/leader-map keys))
        (set-transient-map my/leader-map)
      (let ((command (lookup-key my/leader-map keys)))
        (when command
          (call-interactively command))))))

(global-set-key (kbd "C-SPC") 'my/leader-prefix)

;; Terminal bindings under C-SPC t
(let ((term-map (make-sparse-keymap)))
  (define-key my/leader-map "t" term-map)
  
  ;; Core terminal emulators (1-5 as requested)
  (define-key term-map "1" 'my/terminal-eshell)
  (define-key term-map "2" 'my/terminal-eat)
  (define-key term-map "3" 'my/terminal-shell)
  (define-key term-map "4" 'my/terminal-term)
  (define-key term-map "5" 'my/terminal-ansi-term)
  
  ;; Windows-specific terminals
  (define-key term-map "p" 'my/terminal-powershell)
  (define-key term-map "c" 'my/terminal-cmdproxy)
  
  ;; Additional terminals
  (define-key term-map "w" 'my/terminal-wsl)
  (define-key term-map "z" 'my/terminal-wezterm-integration)
  (define-key term-map "Z" 'my/test-wezterm-ssh)
  
  ;; SSH/MFA testing
  (define-key term-map "s" 'my/test-ssh-mfa)
  (define-key term-map "t" 'my/ssh-tramp-mfa)
  
  ;; Help and debugging
  (define-key term-map "h" 'my/compare-terminals)
  (define-key term-map "?" 'my/terminal-quick-help)
  (define-key term-map "d" 'my/debug-terminal-setup))

;; SSH bindings under C-SPC s
(let ((ssh-map (make-sparse-keymap)))
  (define-key my/leader-map "s" ssh-map)
  
  (define-key ssh-map "d" 'my/ssh-debug-process)
  (define-key ssh-map "w" 'my/ssh-wezterm-setup)
  (define-key ssh-map "a" 'my/test-all-ssh-methods)
  
  ;; Quick access to top methods
  (define-key ssh-map "1" 'my/ssh-method-1-tramp)
  (define-key ssh-map "2" 'my/ssh-method-2-wezterm-shell)
  (define-key ssh-map "3" 'my/ssh-method-3-eat))

;; Other leader bindings
(define-key my/leader-map "f" 'my/fix-child-process-error)
(define-key my/leader-map "T" 'my/test-all-terminals)

;; Quick SSH test - use "S" (uppercase) to avoid conflict with ssh-map
(define-key my/leader-map "S" 'my/quick-ssh-test)

;;==============================================================================
;; DEBUGGING AND UTILITY BINDINGS
;;==============================================================================

(defun my/debug-terminal-setup ()
  "Debug terminal setup and show configuration."
  (interactive)
  (let ((buf (get-buffer-create "*terminal-debug*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "TERMINAL DEBUG INFORMATION\n")
      (insert "===========================\n\n")
      
      (insert "System Info:\n")
      (insert (format "System type: %s\n" system-type))
      (insert (format "Is Windows: %s\n" my/is-windows))
      (insert (format "User: %s\n\n" (user-login-name)))
      
      (insert "Shell Configuration:\n")
      (insert (format "shell-file-name: %s\n" shell-file-name))
      (insert (format "explicit-shell-file-name: %s\n" explicit-shell-file-name))
      (insert (format "shell-command-switch: %s\n\n" shell-command-switch))
      
      (insert "PATH Environment:\n")
      (insert (format "PATH: %s\n\n" (getenv "PATH")))
      
      (insert "PACKAGE MANAGEMENT\n")
      (insert "=================\n\n")
      
      (insert "Package Archives Configured:\n")
      (if package-archives
          (dolist (archive package-archives)
            (insert (format "• %s: %s\n" (car archive) (cdr archive))))
        (insert "✗ No package archives configured!\n"))
      (insert "\n")
      
      (insert "Essential Packages Status:\n")
      (when (boundp 'my/essential-packages)
        (dolist (pkg my/essential-packages)
          (let ((installed (package-installed-p pkg))
                (available (assoc pkg package-archive-contents))
                (loaded (require pkg nil t)))
            (insert (format "• %s: " pkg))
            (cond
             ((and installed loaded) (insert "✓ Installed & loaded\n"))
             (installed (insert "✓ Installed (not loaded)\n"))
             (available (insert "○ Available in archives\n"))
             (t (insert "✗ Not in archives\n"))))))
      (insert "\n")
      
      (insert "Eat Package Details:\n")
      (let ((eat-dir (locate-library "eat")))
        (if eat-dir
            (progn
              (insert (format "✓ Eat library found: %s\n" eat-dir))
              (insert (format "  Function 'eat' bound: %s\n" (fboundp 'eat)))
              (insert (format "  Autoloads file: %s\n" (locate-library "eat-autoloads"))))
          (insert "✗ Eat library not found in load path\n")))
      (insert "\n")
      
      (insert "Common Windows Shell Paths:\n")
      (dolist (path '("C:/Program Files/Git/bin/bash.exe"
                      "C:/Windows/System32/WindowsPowerShell/v1.0/powershell.exe"
                      "C:/Windows/System32/cmd.exe"
                      "C:/Windows/System32/cmdproxy.exe"))
        (insert (format "%s: %s\n" path (if (file-exists-p path) "EXISTS" "NOT FOUND"))))
      
      (insert "\nTRAMP Configuration:\n")
      (insert (format "tramp-default-method: %s\n" (bound-and-true-p tramp-default-method)))
      (insert (format "tramp-default-user: %s\n\n" (bound-and-true-p tramp-default-user)))
      
      (insert "RECOMMENDED ACTIONS:\n")
      (insert "1. If eat not installed: M-x package-refresh-contents then M-x package-install eat\n")
      (insert "2. If package archives empty: Check network or try different mirror\n")
      (insert "3. Use TRAMP for SSH with MFA (C-SPC t t) - works without eat\n")
      (insert "4. Ensure Git for Windows is installed at C:/Program Files/Git/\n")
      (insert "5. Run Emacs inside Wezterm for best terminal emulation\n")
      (insert "6. Press F8 to fix 'child process invalid argument' errors\n")
      
      (special-mode))
    (switch-to-buffer buf)))

(defun my/quick-ssh-test ()
  "Quick SSH test with recommended settings."
  (interactive)
  (message "Quick SSH test - using TRAMP (recommended for MFA)")
  (call-interactively 'my/ssh-tramp-mfa))

(defun my/test-all-terminals ()
  "Open all available terminals for testing.
Creates a buffer with links to test each terminal."
  (interactive)
  (let ((buf (get-buffer-create "*all-terminals-test*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "TEST ALL TERMINALS\n")
      (insert "=================\n\n")
      (insert "Click or press RET on any line to test that terminal:\n\n")
      
      (dolist (terminal '(("eshell" . my/terminal-eshell)
                          ("eat" . my/terminal-eat)
                          ("shell" . my/terminal-shell)
                          ("term" . my/terminal-term)
                          ("ansi-term" . my/terminal-ansi-term)
                          ("powershell" . my/terminal-powershell)
                          ("cmdproxy" . my/terminal-cmdproxy)
                          ("WSL" . my/terminal-wsl)
                          ("Wezterm integration" . my/terminal-wezterm-integration)
                          ("Wezterm SSH test" . my/test-wezterm-ssh)))
        (let ((name (car terminal))
              (func (cdr terminal)))
          (insert (format "[[elisp:(funcall '%s)][%s]]\n" func name))))
      
      (insert "\nRECOMMENDED FOR SSH/MFA:\n")
      (insert "[[elisp:(call-interactively 'my/ssh-tramp-mfa)][TRAMP SSH with MFA]]\n")
      (insert "[[elisp:(call-interactively 'my/test-ssh-mfa)][Test SSH in any terminal]]\n")
      (insert "[[elisp:(my/test-all-ssh-methods)][Test all 10 SSH methods]]\n")
      
      (org-mode))
    (switch-to-buffer buf)))

(provide 'keybindings)