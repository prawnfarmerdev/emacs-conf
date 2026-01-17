;;; helpers.el --- Helper functions for Emacs configuration

;;; Commentary:
;; Helper functions used throughout the configuration.

;;==============================================================================
;; HELPER FUNCTIONS
;;==============================================================================

(defun my/current-dir ()
  "Return directory of current buffer, or `default-directory`."
  (if-let ((file (buffer-file-name)))
      (file-name-directory file)
    default-directory))

(defun my/open-current-dir-dired ()
  "Open dired in current buffer's directory."
  (interactive)
  (dired (my/current-dir)))

(defun my/open-vterm-here ()
  "Open eshell in current buffer's directory.
Uses eshell for cross-platform compatibility."
  (interactive)
  (let ((default-directory (my/current-dir)))
    (eshell)))

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
Uses regexp or fixed strings based on `my/search-use-regexp`."
  (interactive)
  (let ((args (if (boundp 'consult-ripgrep-args)
                  (if my/search-use-regexp
                      consult-ripgrep-args
                    (cons "--fixed-strings" consult-ripgrep-args))
                '("rg" "--null" "--line-buffered" "--color=never" "--max-columns=1000"
                  "--path-separator" "/" "--smart-case" "--no-heading" "--line-number"
                  "--hidden" "-g" "!.git/" "--" "."))))
    (let ((consult-ripgrep-args args))
      (consult-ripgrep (my/current-dir)))))

(defun my/consult-find-current-dir ()
  "Run consult-find in current directory for fuzzy file search."
  (interactive)
  (consult-find (my/current-dir)))

(provide 'helpers)
;;; helpers.el ends here