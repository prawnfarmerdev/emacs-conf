;;; core.el --- Core settings for terminal testing -*- lexical-binding: t -*-

;;; Commentary:
;; Basic Emacs configuration with focus on fixing "child process invalid argument"
;; and Windows compatibility.

;; Platform detection (fallback if not defined by init.el)
(unless (boundp 'my/is-windows)
  (defvar my/is-windows (eq system-type 'windows-nt)
    "Non-nil if running on Windows."))

;;==============================================================================
;; BASIC SETTINGS
;;==============================================================================

;; Disable startup screen
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; UI cleanup
(menu-bar-mode -1)
(tool-bar-mode -1) 
(scroll-bar-mode -1)

;; Better defaults
(setq-default cursor-type 'bar)
(blink-cursor-mode 0)
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; Line numbers
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

;; Font
(when (display-graphic-p)
  (set-face-attribute 'default nil :family "Consolas" :height 110))

;;==============================================================================
;; PACKAGE MANAGEMENT
;;==============================================================================

;; Update package list if needed
(when (boundp 'package-archive-contents)
  (unless package-archive-contents
    (package-refresh-contents)))

;; Essential packages for terminal testing
(defvar my/essential-packages
  '(evil
    general
    which-key
    vertico
    orderless
    consult
    embark
    marginalia
    perspective
    magit
    eat)
  "Essential packages for terminal testing.")

;; Install missing packages
(dolist (package my/essential-packages)
  (unless (package-installed-p package)
    (message "Installing %s..." package)
    (package-install package)))

;;==============================================================================
;; WINDOWS-SPECIFIC FIXES
;;==============================================================================

(when my/is-windows
  (message "Windows detected - applying fixes")
  
  ;; Detect Wezterm for optimal configuration
  (let ((term (getenv "TERM_PROGRAM"))
        (wezterm-dir (getenv "WEZTERM_EXECUTABLE_DIR")))
    (if (or (string= term "WezTerm") wezterm-dir)
        (progn
          (message "Wezterm detected - using optimal configuration")
          ;; Wezterm works best with Git bash
          (setq shell-file-name "C:/Program Files/Git/bin/bash.exe")
          (setq explicit-shell-file-name shell-file-name)
          (setq explicit-bash.exe-args '("--login" "-i"))
          (setq shell-command-switch "-c"))
      
      ;; Not in Wezterm - try multiple configurations
      (message "Not in Wezterm - trying multiple shell configurations")
      ;; First try Git bash
      (if (file-exists-p "C:/Program Files/Git/bin/bash.exe")
          (progn
            (setq shell-file-name "C:/Program Files/Git/bin/bash.exe")
            (setq explicit-shell-file-name shell-file-name))
        ;; Fall back to PowerShell
        (setq shell-file-name "C:/Windows/System32/WindowsPowerShell/v1.0/powershell.exe")
        (setq explicit-shell-file-name shell-file-name)
        (setq shell-command-switch "-Command"))))
  
  ;; Fix shell arguments to avoid "invalid argument" error
  ;; This is critical for Windows process creation
  (unless shell-command-switch
    (setq shell-command-switch "-c"))
  
  ;; Ensure process-connection-type is nil for better pipe handling
  (setq process-connection-type nil)
  
  ;; Fix PATH inheritance - ensure Git/bin is in PATH
  (let ((git-path "C:/Program Files/Git/bin"))
    (when (and (file-exists-p git-path)
               (not (string-match-p (regexp-quote git-path) (getenv "PATH"))))
      (setenv "PATH" (concat git-path ";" (getenv "PATH")))))
  
  ;; TRAMP settings for Windows SSH
  (setq tramp-default-method "plink")
  (setq tramp-default-user (user-login-name))
  
  ;; Additional Windows process fixes
  (setq w32-quote-process-args t)
  (setq w32-pipe-read-delay 0)
  (setq w32-pipe-buffer-size (* 64 1024)))

;;==============================================================================
;; PROCESS/TERMINAL DEBUGGING
;;==============================================================================

(defun my/debug-process-error (orig-fun &rest args)
  "Debug wrapper for process functions to catch 'invalid argument' errors."
  (condition-case err
      (apply orig-fun args)
    (error
     (message "Process error in %s: %S" orig-fun err)
     (signal (car err) (cdr err)))))

;; Wrap common process functions for debugging
(advice-add 'make-process :around #'my/debug-process-error)
(advice-add 'start-process :around #'my/debug-process-error)
(advice-add 'call-process :around #'my/debug-process-error)

(defun my/fix-child-process-error ()
  "Comprehensive fix for 'child process invalid argument' error on Windows.
Tests and applies multiple fixes for Windows process creation issues."
  (interactive)
  (let ((buf (get-buffer-create "*process-fix*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "FIXING 'CHILD PROCESS INVALID ARGUMENT' ERROR\n")
      (insert "============================================\n\n")
      
      (insert "COMMON CAUSES:\n")
      (insert "1. Incorrect shell-file-name or explicit-shell-file-name\n")
      (insert "2. Missing or incorrect shell-command-switch\n")
      (insert "3. PATH issues (Git/bin not in PATH)\n")
      (insert "4. Wezterm not detected properly\n")
      (insert "5. Windows process creation limits\n\n")
      
      (insert "APPLYING FIXES...\n\n")
      
      ;; Fix 1: Shell configuration
      (insert "FIX 1: Shell configuration\n")
      (let ((git-bash "C:/Program Files/Git/bin/bash.exe")
            (powershell "C:/Windows/System32/WindowsPowerShell/v1.0/powershell.exe"))
        (cond
         ((file-exists-p git-bash)
          (setq shell-file-name git-bash)
          (setq explicit-shell-file-name git-bash)
          (setq shell-command-switch "-c")
          (insert (format "✓ Using Git bash: %s\n" git-bash)))
         ((file-exists-p powershell)
          (setq shell-file-name powershell)
          (setq explicit-shell-file-name powershell)
          (setq shell-command-switch "-Command")
          (insert (format "✓ Using PowerShell: %s\n" powershell)))
         (t
          (insert "✗ No valid shell found!\n"))))
      
      ;; Fix 2: PATH check
      (insert "\nFIX 2: PATH environment\n")
      (let ((git-path "C:/Program Files/Git/bin"))
        (if (file-exists-p git-path)
            (progn
              (unless (string-match-p (regexp-quote git-path) (getenv "PATH"))
                (setenv "PATH" (concat git-path ";" (getenv "PATH"))))
              (insert "✓ Git/bin added to PATH\n"))
          (insert "✗ Git/bin not found\n")))
      
      ;; Fix 3: Wezterm detection
      (insert "\nFIX 3: Wezterm detection\n")
      (let ((term (getenv "TERM_PROGRAM"))
            (wezterm-dir (getenv "WEZTERM_EXECUTABLE_DIR")))
        (if (or (string= term "WezTerm") wezterm-dir)
            (progn
              (insert "✓ Running in Wezterm - optimal for SSH\n")
              (insert "  Consider using TRAMP (C-SPC t t) for SSH/MFA\n"))
          (insert "✗ Not in Wezterm - run Emacs from Wezterm for best results\n")))
      
      ;; Fix 4: Windows process settings
      (insert "\nFIX 4: Windows process settings\n")
      (setq w32-quote-process-args t)
      (setq w32-pipe-read-delay 0)
      (setq w32-pipe-buffer-size (* 64 1024))
      (setq process-connection-type nil)
      (insert "✓ Windows process settings optimized\n")
      
      ;; Fix 5: TRAMP configuration
      (insert "\nFIX 5: TRAMP configuration\n")
      (setq tramp-default-method "plink")
      (setq tramp-default-user (user-login-name))
      (insert "✓ TRAMP configured for Windows SSH\n")
      
      ;; Test
      (insert "\nTEST: Basic process test\n")
      (condition-case err
          (progn
            (call-process shell-file-name nil nil nil shell-command-switch "echo 'Test ok'")
            (insert "✓ Process creation successful\n"))
        (error
         (insert (format "✗ Process error: %s\n" (error-message-string err)))))
      
      (insert "\nRECOMMENDATIONS:\n")
      (insert "1. Run Emacs inside Wezterm (fixes many issues)\n")
      (insert "2. Use TRAMP for SSH (C-SPC t t) instead of shell\n")
      (insert "3. If using shell, ensure Git for Windows is installed\n")
      (insert "4. Try eat terminal (C-SPC t 2) if available\n")
      (insert "5. Test all SSH methods: M-x my/test-all-ssh-methods\n")
      
      (special-mode))
    (switch-to-buffer buf)))

;;==============================================================================
;; MODULE SETUP
;;==============================================================================

;; Load completion framework
(when (require 'vertico nil t)
  (vertico-mode))

(when (require 'orderless nil t)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Load which-key for keybinding hints
(when (require 'which-key nil t)
  (which-key-mode))

;; Load general for keybindings
(unless (require 'general nil t)
  (message "Warning: general package not available"))

;; Startup message
(add-hook 'emacs-startup-hook
          (lambda ()
            (when (interactive-p)
              (message "Terminal testing configuration loaded!")
              (message "Press F1 for help, F8 to fix process errors, F9 to test SSH methods")
              (message "Use C-SPC t 1-5 to test terminals, C-SPC t t for TRAMP SSH"))))

(provide 'core)