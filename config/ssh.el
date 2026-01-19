;;; ssh.el --- SSH configuration for Windows/Wezterm testing -*- lexical-binding: t -*-

;;; Commentary:
;; SSH configuration focused on fixing "child process invalid argument" error
;; and Wezterm integration for SSH with MFA support.

;;==============================================================================
;; DEBUGGING PROCESS ERRORS
;;==============================================================================

(defun my/ssh-debug-process ()
  "Debug SSH process creation to fix 'invalid argument' errors.
Tests different shell configurations and process creation methods."
  (interactive)
  (let ((buf (get-buffer-create "*ssh-process-debug*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "SSH PROCESS DEBUGGING\n")
      (insert "====================\n\n")
      
      (insert "Testing shell configurations...\n\n")
      
      ;; Test 1: Basic shell test
      (insert "TEST 1: Basic shell test\n")
      (condition-case err
          (progn
            (call-process shell-file-name nil nil nil "-c" "echo 'Shell test ok'")
            (insert "✓ Shell works: " shell-file-name "\n"))
        (error
         (insert "✗ Shell error: " (error-message-string err) "\n")))
      
      ;; Test 2: Explicit shell
      (insert "\nTEST 2: Explicit shell\n")
      (condition-case err
          (progn
            (call-process explicit-shell-file-name nil nil nil "-c" "echo 'Explicit shell ok'")
            (insert "✓ Explicit shell works: " explicit-shell-file-name "\n"))
        (error
         (insert "✗ Explicit shell error: " (error-message-string err) "\n")))
      
      ;; Test 3: SSH command test
      (insert "\nTEST 3: SSH command test\n")
      (condition-case err
          (progn
            (call-process shell-file-name nil nil nil "-c" "ssh -V 2>&1 | head -1")
            (insert "✓ SSH version check ok\n"))
        (error
         (insert "✗ SSH error: " (error-message-string err) "\n")))
      
      ;; Test 4: Wezterm detection
      (insert "\nTEST 4: Wezterm environment\n")
      (let ((term (getenv "TERM_PROGRAM"))
            (wezterm-dir (getenv "WEZTERM_EXECUTABLE_DIR")))
        (insert (format "TERM_PROGRAM: %s\n" term))
        (insert (format "WEZTERM_EXECUTABLE_DIR: %s\n" wezterm-dir))
        (if (or (string= term "WezTerm") wezterm-dir)
            (insert "✓ Running in Wezterm - SSH should work better\n")
          (insert "✗ Not in Wezterm - consider running Emacs from Wezterm\n")))
      
      ;; Test 5: PATH check
      (insert "\nTEST 5: PATH environment\n")
      (let ((path (getenv "PATH")))
        (insert (format "PATH length: %d chars\n" (length path)))
        (insert (format "Contains Git: %s\n" (string-match-p "Git" path)))
        (insert (format "Contains ssh: %s\n" (string-match-p "ssh" path))))
      
      ;; Recommendations
      (insert "\nRECOMMENDATIONS:\n")
      (insert "1. Run Emacs inside Wezterm for best SSH support\n")
      (insert "2. Use TRAMP for SSH with MFA (C-SPC t t)\n")
      (insert "3. If using shell, ensure shell-file-name is correct\n")
      (insert "4. Check PATH includes Git/bin and SSH\n")
      (insert "5. Try eat terminal (C-SPC t 2) if available\n")
      
      (special-mode))
    (switch-to-buffer buf)))

;;==============================================================================
;; WEZTERM-SPECIFIC SSH SETUP
;;==============================================================================

(defun my/ssh-wezterm-setup ()
  "Configure SSH for Wezterm environment.
Sets up optimal configuration when running inside Wezterm."
  (interactive)
  (let ((term (getenv "TERM_PROGRAM")))
    (if (string= term "WezTerm")
        (progn
          (message "Configuring SSH for Wezterm...")
          
          ;; Wezterm-specific settings
          (setq shell-file-name "C:/Program Files/Git/bin/bash.exe")
          (setq explicit-shell-file-name shell-file-name)
          (setq shell-command-switch "-c")
          
          ;; Add Git to PATH if not already there
          (let ((git-path "C:/Program Files/Git/bin"))
            (unless (string-match-p (regexp-quote git-path) (getenv "PATH"))
              (setenv "PATH" (concat git-path ";" (getenv "PATH")))))
          
          ;; TRAMP settings for Wezterm
          (setq tramp-default-method "plink")
          (setq tramp-default-user (user-login-name))
          
          (message "Wezterm SSH configuration complete"))
      (message "Not running in Wezterm - configuration skipped"))))

;;==============================================================================
;; SSH CONNECTION METHODS (10 SETUPS)
;;==============================================================================

(defun my/ssh-method-1-tramp ()
  "Method 1: TRAMP (recommended for MFA).
Uses Emacs TRAMP for SSH connections."
  (interactive)
  (message "Method 1: TRAMP SSH")
  (call-interactively 'my/ssh-tramp-mfa))

(defun my/ssh-method-2-wezterm-shell ()
  "Method 2: Wezterm shell.
Uses shell inside Wezterm for SSH."
  (interactive)
  (message "Method 2: Wezterm shell SSH")
  (my/test-wezterm-ssh))

(defun my/ssh-method-3-eat ()
  "Method 3: eat terminal.
Uses eat terminal emulator for SSH."
  (interactive)
  (message "Method 3: eat terminal SSH")
  (if (require 'eat nil t)
      (let ((buf (my/terminal-eat)))
        (when buf
          (with-current-buffer buf
            (goto-char (point-max))
            (insert "echo 'eat SSH test - try: ssh user@host'\n")
            (comint-send-input))))
    (message "eat package not available")))

(defun my/ssh-method-4-shell ()
  "Method 4: Standard shell.
Uses default shell for SSH (may have 'invalid argument' error)."
  (interactive)
  (message "Method 4: Standard shell SSH")
  (let ((buf (my/terminal-shell)))
    (when buf
      (with-current-buffer buf
        (goto-char (point-max))
        (insert "echo 'Standard shell SSH test'\n")
        (insert "echo 'If you see invalid argument error, try Wezterm'\n")
        (comint-send-input)))))

(defun my/ssh-method-5-eshell ()
  "Method 5: eshell.
Uses Emacs eshell for SSH (limited terminal emulation)."
  (interactive)
  (message "Method 5: eshell SSH")
  (let ((buf (my/terminal-eshell)))
    (when buf
      (with-current-buffer buf
        (goto-char (point-max))
        (insert "echo 'eshell SSH test'")
        (eshell-send-input)))))

(defun my/ssh-method-6-term ()
  "Method 6: term.
Uses full terminal emulation (may not work well on Windows)."
  (interactive)
  (message "Method 6: term SSH")
  (let ((buf (my/terminal-term)))
    (when buf
      (with-current-buffer buf
        (goto-char (point-max))
        (term-send-string buf "echo 'term SSH test'\n")))))

(defun my/ssh-method-7-ansi-term ()
  "Method 7: ansi-term.
ANSI terminal emulation (similar to term with color)."
  (interactive)
  (message "Method 7: ansi-term SSH")
  (let ((buf (my/terminal-ansi-term)))
    (when buf
      (with-current-buffer buf
        (goto-char (point-max))
        (term-send-string buf "echo 'ansi-term SSH test'\n")))))

(defun my/ssh-method-8-powershell ()
  "Method 8: PowerShell.
Windows PowerShell SSH (Windows only)."
  (interactive)
  (message "Method 8: PowerShell SSH")
  (let ((buf (my/terminal-powershell)))
    (when buf
      (with-current-buffer buf
        (goto-char (point-max))
        (insert "Write-Host 'PowerShell SSH test'\n")
        (comint-send-input)))))

(defun my/ssh-method-9-wsl ()
  "Method 9: WSL.
Windows Subsystem for Linux SSH (requires WSL)."
  (interactive)
  (message "Method 9: WSL SSH")
  (let ((buf (my/terminal-wsl)))
    (when buf
      (with-current-buffer buf
        (goto-char (point-max))
        (term-send-string buf "echo 'WSL SSH test'\n")))))

(defun my/ssh-method-10-cmdproxy ()
  "Method 10: cmdproxy.
Windows cmdproxy SSH (better process handling)."
  (interactive)
  (message "Method 10: cmdproxy SSH")
  (let ((buf (my/terminal-cmdproxy)))
    (when buf
      (with-current-buffer buf
        (goto-char (point-max))
        (insert "echo 'cmdproxy SSH test'\n")
        (comint-send-input)))))

;;==============================================================================
;; SSH TEST RUNNER
;;==============================================================================

(defun my/test-all-ssh-methods ()
  "Test all 10 SSH methods.
Creates buffer with links to test each method."
  (interactive)
  (let ((buf (get-buffer-create "*all-ssh-methods*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "TEST ALL SSH METHODS (10 SETUPS)\n")
      (insert "================================\n\n")
      (insert "Click or press RET on any line to test that SSH method:\n\n")
      
      (dolist (method '(("1. TRAMP (RECOMMENDED for MFA)" . my/ssh-method-1-tramp)
                        ("2. Wezterm shell" . my/ssh-method-2-wezterm-shell)
                        ("3. eat terminal" . my/ssh-method-3-eat)
                        ("4. Standard shell" . my/ssh-method-4-shell)
                        ("5. eshell" . my/ssh-method-5-eshell)
                        ("6. term" . my/ssh-method-6-term)
                        ("7. ansi-term" . my/ssh-method-7-ansi-term)
                        ("8. PowerShell" . my/ssh-method-8-powershell)
                        ("9. WSL" . my/ssh-method-9-wsl)
                        ("10. cmdproxy" . my/ssh-method-10-cmdproxy)))
        (let ((name (car method))
              (func (cdr method)))
          (insert (format "[[elisp:(funcall '%s)][%s]]\n" func name))))
      
      (insert "\nDEBUGGING:\n")
      (insert "[[elisp:(my/ssh-debug-process)][Debug SSH process errors]]\n")
      (insert "[[elisp:(my/ssh-wezterm-setup)][Configure Wezterm SSH]]\n")
      (insert "[[elisp:(my/debug-terminal-setup)][Debug terminal setup]]\n")
      
      (org-mode))
    (switch-to-buffer buf)))

;;==============================================================================
;; KEYBINDINGS (defined in keybindings.el)
;;==============================================================================

;; Note: SSH keybindings are defined in keybindings.el under C-SPC s
;; This includes:
;; - C-SPC s d: Debug SSH process
;; - C-SPC s w: Wezterm SSH setup  
;; - C-SPC s a: Test all SSH methods
;; - C-SPC s 1-3: Quick access to top SSH methods

;; Auto-configure Wezterm on load if detected
(when (and my/is-windows (string= (getenv "TERM_PROGRAM") "WezTerm"))
  (my/ssh-wezterm-setup))

(provide 'ssh)