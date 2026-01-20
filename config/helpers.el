;;; helpers.el --- Terminal helper functions for testing -*- lexical-binding: t -*-

;;; Commentary:
;; Terminal testing functions for Windows SSH/MFA support
;; C-SPC t 1-5: Test different terminal emulators

;;==============================================================================
;; TERMINAL TEST FUNCTIONS
;;==============================================================================

(defun my/terminal-eshell ()
  "Open eshell terminal.
Eshell is Emacs' built-in shell written in Elisp.
Pros: Integrated with Emacs, cross-platform
Cons: Limited terminal emulation, may have issues with SSH MFA"
  (interactive)
  (message "Opening eshell...")
  (require 'eshell)
  (let ((buf (eshell)))
    (with-current-buffer buf
      (rename-buffer "*eshell-test*" t))
    buf))

(defun my/terminal-eat ()
  "Open eat terminal emulator.
Eat is a modern terminal emulator within Emacs.
Pros: Good terminal emulation, supports SSH
Cons: Requires eat package"
  (interactive)
  (message "Opening eat terminal...")
  
  ;; Try multiple ways to load eat
  (cond
   ;; First try standard require
   ((require 'eat nil t)
    (eat)
    (rename-buffer "*eat-test*" t)
    t)
   
   ;; Try loading from autoloads
   ((load "eat-autoloads" nil t)
    (if (fboundp 'eat)
        (progn
          (eat)
          (rename-buffer "*eat-test*" t)
          t)
      (message "eat autoloads loaded but 'eat' function not found")
      nil))
   
   ;; Try to install package
   (t
    (let ((buf (get-buffer-create "*eat-install-help*")))
      (with-current-buffer buf
        (erase-buffer)
        (insert "EAT PACKAGE NOT AVAILABLE\n")
        (insert "=========================\n\n")
        (insert "The 'eat' terminal emulator is not installed.\n\n")
        (insert "TO INSTALL:\n")
        (insert "1. M-x package-refresh-contents RET\n")
        (insert "2. M-x package-install RET eat RET\n\n")
        (insert "ALTERNATIVES:\n")
        (insert "• Use TRAMP SSH (C-SPC t t) - recommended for MFA\n")
        (insert "• Try eshell (C-SPC t 1)\n")
        (insert "• Try shell (C-SPC t 3)\n")
        (insert "• Try term (C-SPC t 4)\n")
        (insert "• Try ansi-term (C-SPC t 5)\n\n")
        (insert "PACKAGE ARCHIVES CONFIGURED:\n")
        (dolist (archive package-archives)
          (insert (format "• %s: %s\n" (car archive) (cdr archive))))
        (special-mode))
      (switch-to-buffer buf)
      nil))))

(defun my/terminal-shell ()
  "Open shell (inferior shell process).
Uses system shell (bash on Unix, cmd/powershell on Windows).
Pros: Native shell, good for SSH
Cons: Process may have 'invalid argument' errors"
  (interactive)
  (message "Opening shell...")
  (let ((buf (shell)))
    (with-current-buffer buf
      (rename-buffer "*shell-test*" t))
    buf))

(defun my/terminal-term ()
  "Open term (full terminal emulator).
Full terminal emulation using term.el.
Pros: Full terminal emulation, supports SSH
Cons: May not work well with Windows"
  (interactive)
  (message "Opening term...")
  (let ((buf (term (or explicit-shell-file-name shell-file-name))))
    (with-current-buffer buf
      (rename-buffer "*term-test*" t))
    buf))

(defun my/terminal-ansi-term ()
  "Open ansi-term (ANSI terminal emulator).
Similar to term but with ANSI support.
Pros: ANSI color support, good terminal emulation
Cons: May have issues on Windows"
  (interactive)
  (message "Opening ansi-term...")
  (let ((buf (ansi-term (or explicit-shell-file-name shell-file-name))))
    (with-current-buffer buf
      (rename-buffer "*ansi-term-test*" t))
    buf))

(defun my/terminal-powershell ()
  "Open PowerShell terminal (Windows only).
Direct PowerShell terminal for Windows users.
Pros: Native PowerShell, good for Windows SSH
Cons: Windows only"
  (interactive)
  (message "Opening PowerShell...")
  (if my/is-windows
      (let ((buf (shell)))
        (with-current-buffer buf
          (rename-buffer "*powershell-test*" t)
          (when (string-match-p "powershell" (or shell-file-name ""))
            (term-send-string buf "powershell\n")))
        buf)
    (message "PowerShell only available on Windows")
    nil))

(defun my/terminal-cmdproxy ()
  "Open cmd.exe proxy (Windows only).
Uses cmdproxy for better Windows compatibility.
Pros: Better Windows process handling
Cons: Windows only, cmdproxy may not be installed"
  (interactive)
  (message "Opening cmdproxy...")
  (if my/is-windows
      (let* ((cmdproxy "C:/Windows/System32/cmdproxy.exe")
             (buf (if (file-exists-p cmdproxy)
                      (progn
                        (setq explicit-shell-file-name cmdproxy)
                        (shell))
                    (message "cmdproxy.exe not found at %s" cmdproxy)
                    nil))))
    (message "cmdproxy only available on Windows")
    nil))

(defun my/terminal-vterm ()
  "Open vterm terminal emulator.
vterm is a fast terminal emulator using libvterm.
Requires: vterm package and libvterm library."
  (interactive)
  (message "Opening vterm...")
  (if (require 'vterm nil t)
      (progn
        (vterm)
        (rename-buffer "*vterm-test*" t))
    (message "vterm package not available - install with M-x package-install RET vterm RET")
    nil))

(defun my/terminal-multi-term ()
  "Open multi-term terminal.
Manages multiple term buffers."
  (interactive)
  (message "Opening multi-term...")
  (if (require 'multi-term nil t)
      (progn
        (multi-term)
        (rename-buffer "*multi-term-test*" t))
    (message "multi-term package not available - install with M-x package-install RET multi-term RET")
    nil))

(defun my/terminal-wsl ()
  "Open WSL (Windows Subsystem for Linux) terminal.
Requires WSL installation and bash.exe."
  (interactive)
  (message "Opening WSL terminal...")
  (if my/is-windows
      (let ((wsl-bash "C:/Windows/System32/bash.exe"))
        (if (file-exists-p wsl-bash)
            (let ((buf (term wsl-bash)))
              (with-current-buffer buf
                (rename-buffer "*wsl-test*" t))
              buf)
          (message "WSL bash.exe not found at %s" wsl-bash)
          nil))
    (message "WSL only available on Windows")
    nil))

(defun my/terminal-wezterm-integration ()
  "Test Wezterm integration when Emacs runs inside Wezterm.
If Emacs is running inside Wezterm, SSH may work better.
This function tests if we're in Wezterm and provides recommendations."
  (interactive)
  (let ((buf (get-buffer-create "*wezterm-test*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "WEZTERM INTEGRATION TEST\n")
      (insert "========================\n\n")
      
      ;; Check if we're in Wezterm
      (let ((term (getenv "TERM_PROGRAM"))
            (wezterm-dir (getenv "WEZTERM_EXECUTABLE_DIR")))
        (insert (format "TERM_PROGRAM: %s\n" term))
        (insert (format "WEZTERM_EXECUTABLE_DIR: %s\n\n" wezterm-dir))
        
        (if (or (string= term "WezTerm") wezterm-dir)
            (progn
              (insert "✓ Running inside Wezterm detected!\n\n")
              (insert "RECOMMENDATIONS:\n")
              (insert "1. Wezterm handles SSH/MFA prompts well\n")
              (insert "2. Use native shell (C-SPC t 3) for SSH\n")
              (insert "3. Or use TRAMP (C-SPC t t) for Emacs integration\n")
              (insert "4. Wezterm's terminal emulation is excellent\n\n")
              (insert "SETUP:\n")
              (insert "- Ensure Wezterm is your terminal\n")
              (insert "- Emacs should inherit proper environment\n")
              (insert "- SSH may work better than in other terminals\n"))
          (insert "Not running inside Wezterm\n")
          (insert "Consider running Emacs inside Wezterm for better SSH support\n")))
      
      (insert "\nTEST SSH IN WEZTERM:\n")
      (insert "1. Open shell (C-SPC t 3)\n")
      (insert "2. Try SSH command: ssh user@host\n")
      (insert "3. Wezterm should handle MFA prompts\n\n")
      
      (insert "WEZTERM ADVANTAGES:\n")
      (insert "- Excellent terminal emulation\n")
      (insert "- Good Windows process handling\n")
      (insert "- Better SSH/MFA support than cmd/powershell\n")
      (insert "- Modern features (GPU acceleration, etc.)\n")
      
      (special-mode))
    (switch-to-buffer buf)))

(defun my/test-wezterm-ssh ()
  "Test SSH specifically in Wezterm environment.
Opens shell and tries SSH connection with Wezterm detection."
  (interactive)
  (let ((term (getenv "TERM_PROGRAM")))
    (if (string= term "WezTerm")
        (progn
          (message "Wezterm detected - testing SSH...")
          (let ((buf (my/terminal-shell)))
            (when buf
              (with-current-buffer buf
                (goto-char (point-max))
                (insert "echo 'Wezterm SSH test - try: ssh user@host'\n")
                (comint-send-input)))))
      (message "Not running in Wezterm. Start Emacs from Wezterm for best results."))))

;;==============================================================================
;; SSH/MFA TEST FUNCTION
;;==============================================================================

(defun my/test-ssh-mfa (hostname username)
  "Test SSH connection with MFA support.
Opens SSH connection in selected terminal and waits for MFA prompt.
HOSTNAME: Server to connect to
USERNAME: SSH username"
  (interactive "sHostname: \nsUsername: ")
  (message "Testing SSH MFA connection to %s@%s" username hostname)
  
  ;; Let user choose terminal type
  (let* ((choices '(("eshell" . my/terminal-eshell)
                    ("eat" . my/terminal-eat)
                    ("shell" . my/terminal-shell)
                    ("term" . my/terminal-term)
                    ("ansi-term" . my/terminal-ansi-term)
                    ("powershell" . my/terminal-powershell)
                    ("cmdproxy" . my/terminal-cmdproxy)
                    ("WSL" . my/terminal-wsl)))
         (choice (completing-read "Select terminal type: "
                                  (mapcar 'car choices) nil t "eshell"))
         (terminal-func (cdr (assoc choice choices)))
         (buf (funcall terminal-func)))
    
    (when buf
      (with-current-buffer buf
        (goto-char (point-max))
        ;; Send SSH command
        (let ((ssh-cmd (format "ssh %s@%s\n" username hostname)))
          (message "Sending SSH command: %s" ssh-cmd)
          (cond
           ((eq terminal-func 'my/terminal-eshell)
            (insert ssh-cmd)
            (eshell-send-input))
           ((or (eq terminal-func 'my/terminal-term)
                (eq terminal-func 'my/terminal-ansi-term))
            (term-send-string buf ssh-cmd))
           (t
            (insert ssh-cmd)
            (comint-send-input)))))))
  
  (message "SSH MFA test started. Check for MFA prompt in terminal."))

;;==============================================================================
;; TRAMP SSH FUNCTION (RECOMMENDED FOR MFA)
;;==============================================================================

(defun my/ssh-tramp-mfa (hostname username)
  "Connect via TRAMP with MFA support.
TRAMP handles SSH connections within Emacs and supports MFA prompts.
This is the RECOMMENDED method for SSH with MFA on Windows."
  (interactive "sHostname: \nsUsername: ")
  (message "Opening TRAMP connection to %s@%s" username hostname)
  
  (let ((tramp-path (format "/ssh:%s@%s:" username hostname)))
    (cond
     ;; Try dired first (file browser)
     ((y-or-n-p "Open remote directory with dired? (Recommended for file access)")
      (dired tramp-path)
      (message "TRAMP dired opened. For shell access, run: M-x shell RET"))
     
     ;; Or open shell directly
     (t
      (let ((default-directory tramp-path))
        (shell)
        (message "TRAMP shell opened in remote directory"))))))

;;==============================================================================
;; TERMINAL COMPARISON HELPER
;;==============================================================================

(defun my/compare-terminals ()
  "Open comparison buffer showing terminal options and recommendations."
  (interactive)
  (let ((buf (get-buffer-create "*terminal-comparison*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "TERMINAL OPTIONS FOR WINDOWS SSH/MFA\n")
      (insert "=====================================\n\n")
      
      (insert "RECOMMENDED FOR SSH/MFA:\n")
      (insert "1. TRAMP (C-SPC t t) - Best for MFA, integrated with Emacs\n")
      (insert "   - Handles SSH prompts automatically\n")
      (insert "   - File access via dired, shell access via M-x shell\n")
      (insert "   - No terminal emulation issues\n\n")
      
      (insert "TERMINAL EMULATORS (test with C-SPC t 1-5):\n")
      (insert "1. eshell (C-SPC t 1) - Emacs Lisp shell\n")
      (insert "   Pros: Integrated, cross-platform\n")
      (insert "   Cons: Limited terminal emulation\n\n")
      
      (insert "2. eat (C-SPC t 2) - Modern terminal emulator\n")
      (insert "   Pros: Good emulation, SSH support\n")
      (insert "   Cons: Requires package install\n\n")
      
      (insert "3. shell (C-SPC t 3) - Inferior shell process\n")
      (insert "   Pros: Native shell\n")
      (insert "   Cons: May have 'invalid argument' errors\n\n")
      
      (insert "4. term (C-SPC t 4) - Full terminal emulator\n")
      (insert "   Pros: Full emulation\n")
      (insert "   Cons: Windows compatibility issues\n\n")
      
      (insert "5. ansi-term (C-SPC t 5) - ANSI terminal\n")
      (insert "   Similar to term with color support\n\n")
      
      (insert "WINDOWS-SPECIFIC:\n")
      (insert "- powershell (C-SPC t p) - Native PowerShell\n")
      (insert "- cmdproxy (C-SPC t c) - Better process handling\n")
      (insert "- Wezterm integration (C-SPC t z) - Best terminal for SSH\n\n")
      
      (insert "WEZTERM ADVANTAGES FOR SSH:\n")
      (insert "- Excellent terminal emulation\n")
      (insert "- Good Windows process handling (fixes 'invalid argument')\n")
      (insert "- Better SSH/MFA support than cmd/powershell\n")
      (insert "- Modern features (GPU acceleration, etc.)\n\n")
      
      (insert "RECOMMENDED WORKFLOW:\n")
      (insert "1. Run Emacs inside Wezterm\n")
      (insert "2. Use TRAMP for SSH with MFA (C-SPC t t)\n")
      (insert "3. Or use shell inside Wezterm (C-SPC t 3)\n\n")
      
      (insert "TESTING:\n")
      (insert "M-x my/test-ssh-mfa RET - Test SSH with MFA in any terminal\n")
      (insert "M-x my/ssh-tramp-mfa RET - Test SSH with TRAMP (recommended)\n")
      (insert "M-x my/test-wezterm-ssh RET - Test SSH in Wezterm\n")
      (insert "M-x my/test-all-ssh-methods RET - Test all 10 SSH methods\n")
      
      (special-mode))
    (switch-to-buffer buf)))

(defun my/terminal-quick-help ()
  "Show quick help for terminal testing and SSH setup."
  (interactive)
  (let ((buf (get-buffer-create "*terminal-quick-help*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "QUICK HELP: TERMINAL TESTING & SSH SETUP\n")
      (insert "========================================\n\n")
      
      (insert "GOAL: Fix 'child process invalid argument' error and get SSH/MFA working\n\n")
      
      (insert "KEYBINDINGS:\n")
      (insert "C-SPC t 1-5 - Test terminal emulators (1:eshell, 2:eat, 3:shell, 4:term, 5:ansi-term)\n")
      (insert "C-SPC t p   - PowerShell terminal\n")
      (insert "C-SPC t c   - cmdproxy terminal\n")
      (insert "C-SPC t w   - WSL terminal\n")
      (insert "C-SPC t z   - Wezterm integration test\n")
      (insert "C-SPC t Z   - Test SSH in Wezterm\n")
      (insert "C-SPC t t   - TRAMP SSH with MFA (RECOMMENDED)\n")
      (insert "C-SPC t s   - Test SSH MFA in any terminal\n")
      (insert "C-SPC t h   - Terminal comparison help\n")
      (insert "C-SPC t d   - Debug terminal setup\n")
      (insert "C-SPC f     - Fix process errors\n")
      (insert "C-SPC s     - Quick SSH test\n")
      (insert "C-SPC T     - Test all terminals\n")
      (insert "C-SPC s a   - Test all 10 SSH methods\n")
      (insert "C-'         - Quick eshell\n")
      (insert "C-\"         - Quick TRAMP SSH\n\n")
      
      (insert "WEZTERM ANSWER:\n")
      (insert "YES! SSH will likely work better if you open Emacs inside Wezterm.\n")
      (insert "Wezterm has excellent terminal emulation and fixes Windows process issues.\n\n")
      
      (insert "RECOMMENDED WORKFLOW:\n")
      (insert "1. Install Wezterm (https://wezfurlong.org/wezterm/)\n")
      (insert "2. Start Emacs from Wezterm: `wezterm start emacs`\n")
      (insert "3. Use TRAMP for SSH: C-SPC t t (hostname, username)\n")
      (insert "4. For shell access: M-x shell (after TRAMP connection)\n\n")
      
      (insert "ALTERNATIVES IF WEZTERM DOESN'T WORK:\n")
      (insert "1. Use eat terminal: C-SPC t 2 (install: M-x package-install eat)\n")
      (insert "2. Use TRAMP directly: C-SPC t t (always works)\n")
      (insert "3. Try different shell: C-SPC t 3, C-SPC t p, C-SPC t c\n\n")
      
      (insert "DEBUGGING:\n")
      (insert "M-x my/fix-child-process-error - Comprehensive fix for 'invalid argument'\n")
      (insert "M-x my/debug-terminal-setup    - Show terminal configuration\n")
      (insert "M-x my/ssh-debug-process       - Debug SSH process creation\n\n")
      
      (insert "NEXT STEPS:\n")
      (insert "1. Test if in Wezterm: C-SPC t z\n")
      (insert "2. Fix process errors: C-SPC f\n")
      (insert "3. Test SSH: C-SPC s a (all 10 methods)\n")
      (insert "4. Use TRAMP for MFA: C-SPC t t\n")
      
      (special-mode))
    (switch-to-buffer buf)))

(provide 'helpers)