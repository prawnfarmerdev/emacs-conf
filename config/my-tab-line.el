;;; my-tab-line.el --- Tab-line configuration with perspective integration

;;; Commentary:
;; Tab-line configuration showing only file buffers in current perspective.

;;==============================================================================
;; TAB-BAR - Simple built-in tab bar with perspective integration
;;==============================================================================

;; tab-line-mode to show only file-visiting buffers in current perspective
(require 'cl-lib)
(require 'seq)
(global-tab-line-mode t)
(setq tab-line-close-button-show nil
      tab-line-new-button-show nil
      tab-line-separator "  ")
;; Filter to show buffers for tab-line, filtered by current perspective.
;; Includes file buffers, dired buffers, and terminal buffers.
(defun my/tab-line-tabs-buffer-list ()
  "Return list of buffers for tab-line, filtered by current perspective.
Includes file buffers, dired buffers, and terminal buffers."
  (let ((buffers (if (and (featurep 'perspective) persp-mode)
                     (persp-get-buffers)
                   (buffer-list))))
    (seq-filter
     (lambda (buf)
       (with-current-buffer buf
         (or (buffer-file-name buf)
             (derived-mode-p 'dired-mode)
             (derived-mode-p 'shell-mode)
             (derived-mode-p 'eshell-mode)
             (derived-mode-p 'term-mode)
             (derived-mode-p 'vterm-mode))))
     buffers)))
(setq tab-line-tabs-function #'my/tab-line-tabs-buffer-list)
;; Custom tab name with numbers
(defun my/tab-line-tab-name-with-number (buffer &optional buffers)
  "Return tab name with number prefix."
  (let* ((tabs (or buffers (funcall tab-line-tabs-function)))
         (index (and tabs (cl-position buffer tabs :test #'eq)))
         (name (cond
                ((buffer-file-name buffer)
                 (file-name-nondirectory (buffer-file-name buffer)))
                ((with-current-buffer buffer
                   (derived-mode-p 'dired-mode))
                 (let ((dir (with-current-buffer buffer dired-directory)))
                   (if (stringp dir)
                       (file-name-nondirectory (directory-file-name dir))
                     (buffer-name buffer))))
                (t
                 (buffer-name buffer)))))
    (if index
        (format "%d:%s" (1+ index) name)
      name)))
(setq tab-line-tab-name-function #'my/tab-line-tab-name-with-number)

;; Simple tab-line format function
(defun my/tab-line-format (tabs)
  "Format TABS for display in tab-line."
  (mapconcat (lambda (buffer)
               (let ((selected-p (eq buffer (current-buffer))))
                 (propertize (funcall tab-line-tab-name-function buffer)
                             'face (if selected-p 'tab-line-tab-current 'tab-line-tab-inactive)
                             'mouse-face 'highlight
                             'local-map (tab-line-make-tab-keymap buffer))))
             tabs
             tab-line-separator))

(setq tab-line-format-function
      (lambda (tabs)
        (list (my/tab-line-format tabs))))
;; Style tab-line to match your solarized theme
(set-face-attribute 'tab-line nil
                    :background "#000000"
                    :foreground "#859900"
                    :height 1.0
                    :box nil)
(set-face-attribute 'tab-line-tab-current nil
                    :background "#000000"
                    :foreground "#2aa198"
                    :weight 'bold
                    :box nil)
(set-face-attribute 'tab-line-tab-inactive nil
                    :background "#000000"
                    :foreground "#586e75"
                    :weight 'normal
                     :box nil)

;; Close current tab in tab-line
(defun my/tab-line-close-current-tab ()
  "Close current tab or switch to last tab for read-only buffers.
If current buffer is a file buffer and not read-only, close it.
Otherwise, switch to most recent file buffer (last tab)."
  (interactive)
  (let ((tabs (funcall tab-line-tabs-function))
        (current (current-buffer)))
    (cond
     ;; Current buffer is a file buffer and not read-only: close it
     ((and (member current tabs)
           (not buffer-read-only))
      (condition-case nil
          (if (fboundp 'tab-line-close-tab)
              (tab-line-close-tab)
            (kill-buffer))
        (error (kill-buffer))))
     ;; Otherwise: switch to most recent file buffer (last tab)
     (t
      (let* ((other-file-buffers (remove current tabs))
             (recent-file-buffer
              (cl-find-if (lambda (buf)
                            (member buf other-file-buffers))
                          (buffer-list))))
        (cond
         (recent-file-buffer
          (switch-to-buffer recent-file-buffer)
          (message "Switched to last tab (read-only buffer)"))
         ((> (length other-file-buffers) 0)
          ;; Switch to first other file buffer
          (switch-to-buffer (car other-file-buffers))
          (message "Switched to file buffer"))
         (t
          (message "No other file buffers to switch to"))))))))

(provide 'my-tab-line)
;;; my-tab-line.el ends here