;;; my-perspective.el --- Perspective workspace management configuration

;;; Commentary:
;; Perspective configuration for tmux-style workspace management.

;;==============================================================================
;; PERSPECTIVE - WORKSPACE MANAGEMENT
;;==============================================================================

(use-package perspective
  :ensure t
  :demand t
  :bind (("C-x k" . persp-kill-buffer*)
         ("C-x C-b" . persp-list-buffers))
  :commands (my/persp-switch-or-create
             my/persp-kill-current
             my/persp-rename-current
             my/persp-list
             my/persp-ibuffer-full
             my/persp-switch-full
             persp-switch
             persp-next
             persp-prev
             persp-switch-last
             persp-switch-quick
             persp-state-save
             persp-state-load)
  :custom
  (persp-mode-prefix-key (kbd "C-x x"))
  :config
  ;; Enable perspective mode
  (persp-mode)
  
  ;; Make sure perspective knows about special buffers
  (setq persp-add-buffer-on-after-change-major-mode t)
  (setq persp-auto-save-opt 0)  ; Don't auto-save perspectives
  (setq persp-show-modestring nil)  ; Don't show perspective in mode-line
  (setq persp-sort 'name)  ; Sort perspectives by name
  (setq persp-set-last-persp-for-new-frames nil)
  (setq persp-switch-wrap t)
  (setq persp-interactive-completion-function #'completing-read)
  (setq persp-state-default-file (expand-file-name "perspectives" user-emacs-directory))
  
  ;; Don't interfere with ibuffer
  (setq persp-suppress-no-prefix-key-warning t)
  
  ;; Helper: Switch to or create perspective by name
  (defun my/persp-switch-or-create (name)
    "Switch to perspective NAME, creating it if it doesn't exist."
    (interactive "sPerspective name: ")
    (let ((existing (member name (persp-names))))
      (persp-switch name)
      (if existing
          (message "Switched to perspective: %s" name)
        (message "Created new perspective: %s" name))))

  ;; Helper: Kill current perspective
  (defun my/persp-kill-current ()
    "Kill current perspective."
    (interactive)
    (let ((name (persp-name (persp-curr))))
      (when (and name (not (string= name persp-initial-frame-name)))
        (persp-kill name)
        (message "Killed perspective: %s" name))))

  ;; Helper: Rename current perspective
  (defun my/persp-rename-current (new-name)
    "Rename current perspective to NEW-NAME."
    (interactive "sNew perspective name: ")
    (let ((old-name (persp-name (persp-curr))))
      (when old-name
        (persp-rename old-name new-name)
        (message "Renamed perspective %s to %s" old-name new-name))))

  ;; Helper: List all perspectives
  (defun my/persp-list ()
    "List all perspectives and show current one."
    (interactive)
    (let ((names (persp-names))
          (current (persp-name (persp-curr))))
      (if names
          (message "Perspectives: %s (current: %s)" 
                   (mapconcat 'identity names ", ") current)
        (message "No perspectives (only main)"))))

  ;; Helper: Full-page perspective ibuffer
  (defun my/persp-ibuffer-full ()
    "Open ibuffer with perspective filtering in full window.
Ensures ibuffer is loaded before calling persp-ibuffer."
    (interactive)
    (require 'ibuffer)
    (delete-other-windows)
    (persp-ibuffer nil))

  ;; Helper: Full-screen perspective switcher
  (defun my/persp-switch-full ()
    "Open full-screen buffer to switch perspectives.
Lists all perspectives with buffer counts for easy switching."
    (interactive)
    (require 'tabulated-list)
    (let ((buffer (get-buffer-create "*Perspectives*")))
      (with-current-buffer buffer
        ;; Make buffer writable for setup
        (setq buffer-read-only nil)
        (erase-buffer)
        (tabulated-list-mode)
        ;; Force emacs state to avoid evil interference
        (when (featurep 'evil) (evil-emacs-state))
        (setq tabulated-list-format
              [("Name" 20 t)
               ("Buffers" 10 t)
               ("Current" 8 t)])
        (setq tabulated-list-entries
              (mapcar (lambda (name)
                        (list name
                              (vector name
                                      (number-to-string (length (persp-get-buffers name)))
                                      (if (string= name (persp-current-name)) "✓" ""))))
                      (persp-names)))
        (tabulated-list-init-header)
        (tabulated-list-print)
        ;; Keybindings: RET select, x delete, g refresh, q quit
        (define-key tabulated-list-mode-map (kbd "RET") #'my/persp-switch-full-select)
        (define-key tabulated-list-mode-map (kbd "x") #'my/persp-switch-full-delete)
        (define-key tabulated-list-mode-map (kbd "g") #'my/persp-switch-full)
        (define-key tabulated-list-mode-map (kbd "q") #'quit-window)
        ;; Make buffer read-only for interaction
        (setq buffer-read-only t))
      (delete-other-windows)
      (switch-to-buffer buffer)))

  ;; Helper: Select perspective from full-screen list
  (defun my/persp-switch-full-select ()
    "Switch to perspective on current line of *Perspectives* buffer."
    (interactive)
    (when (eq (current-buffer) (get-buffer "*Perspectives*"))
      (let ((name (tabulated-list-get-id)))
        (if name
            (progn
              ;; Switch perspective first, then kill buffer
              (persp-switch name)
              (kill-buffer (current-buffer)))
          (message "No perspective at point")))))

  ;; Helper: Delete perspective from full-screen list
  (defun my/persp-switch-full-delete ()
    "Delete perspective on current line of *Perspectives* buffer."
    (interactive)
    (when (eq (current-buffer) (get-buffer "*Perspectives*"))
       (let ((name (tabulated-list-get-id)))
        (when name
          (when (and (not (string= name persp-initial-frame-name))
                     (y-or-n-p (format "Delete perspective '%s'? " name)))
            (persp-kill name)
            (kill-buffer (current-buffer))
            (my/persp-switch-full))))))
  
  ;; Update tab-line when perspective changes
  (add-hook 'persp-switch-hook #'force-mode-line-update)
  (add-hook 'persp-after-rename-hook #'force-mode-line-update))

(provide 'my-perspective)
;;; my-perspective.el ends here