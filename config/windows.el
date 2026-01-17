;;; windows.el --- Windows-specific configuration overrides

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

;; Font fallback chain for Windows
(let ((font (cond
             ((member "JetBrainsMono Nerd Font" (font-family-list))
              "JetBrainsMono Nerd Font")
             ((member "Consolas" (font-family-list))
              "Consolas")
             (t "Courier New"))))
  (setq my/default-font
        (font-spec :family font
                   :weight 'bold
                   :size 12)))
;;==============================================================================
;; WINDOWS TERMINAL CONFIGURATION
;;==============================================================================

;; Use eshell as default terminal on Windows (vterm doesn't work well)
(defun my/open-terminal-here ()
  "Open terminal in current directory (eshell on Windows)."
  (interactive)
  (let ((default-directory (if (bound-and-true-p my/current-dir)
                               (funcall 'my/current-dir)
                             default-directory)))
    (eshell t)))

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

;; Override fzf executable path for Windows
(setq fzf/executable "fzf.exe")

;; Override consult-ripgrep args for Windows
(setq consult-ripgrep-args
      '("rg.exe" "--null" "--line-buffered" "--color=never" "--max-columns=1000"
        "--path-separator" "\\" "--smart-case" "--no-heading" "--line-number"
         "--hidden" "-g" "!.git/" "--" "."))

;; Override fzf grep command for Windows
(setq fzf/grep-command "grep.exe -nrH")

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
;; 1. fzf for Windows: https://github.com/junegunn/fzf/releases
;; 2. ripgrep for Windows: https://github.com/BurntSushi/ripgrep/releases
;; 3. grep for Windows (from GNUWin32 or Git for Windows)
;; 4. PowerShell (already included in Windows 10+)

(provide 'windows)
;;; windows.el ends here