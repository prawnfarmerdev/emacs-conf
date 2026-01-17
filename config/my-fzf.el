;;; my-fzf.el --- FZF fuzzy directory finder configuration

;;; Commentary:
;; FZF configuration for fuzzy file and directory finding.
;; Note: Depends on my-perspective.el for my/persp-switch-or-create
;;       Depends on helpers.el for my/current-dir

;;==============================================================================
;; FZF - FUZZY DIRECTORY FINDER
;;==============================================================================

(use-package fzf
  :ensure t
  :bind (("C-f" . my/emacs-sessionizer)
         ("M-f" . fzf-directory))
  :config
  ;; Requires fzf binary installed on system
  ;; For grep functionality, ripgrep (rg) is recommended
   (setq fzf/args "-x --color=bg:#000000,fg:#839496,hl:#b58900,fg+:#fdf6e3,bg+:#073642,hl+:#b58900 --print-query --margin=1,0 --no-hscroll --height 40%"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        fzf/grep-command "grep -nrH"
        fzf/position-bottom t
         fzf/window-height 15)

  ;; Helper: Get directory candidates from predefined paths
  (defun my/get-directory-candidates ()
    "Return list of subdirectory paths from common directories."
    (let ((roots (list (expand-file-name "~/files")
                       (expand-file-name "~/projects")
                       (expand-file-name "~")
                       (expand-file-name "~/.config")
                       (expand-file-name "~/.local")
                       (expand-file-name "/home")))
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

  ;; Custom fzf functions
  (defun my/fzf-project-dirs ()
    "FZF search through common project directories."
    (interactive)
    (let ((candidates (my/get-directory-candidates)))
      (when candidates
        (fzf-with-entries candidates
                          (lambda (selected)
                            (when selected
                              (dired selected)))
                          nil
                          nil))))
  
   (defun my/fzf-grep ()
     "Run fzf grep in current directory."
     (interactive)
     (let ((default-directory (my/current-dir)))
       (call-interactively 'fzf-grep)))
   
  (defun my/emacs-sessionizer ()
    "FZF directory selection with perspective workspace management.
Switches to perspective named after directory and opens dired."
    (interactive)
    (let ((candidates (my/get-directory-candidates)))
      (when candidates
        (fzf-with-entries candidates
                          (lambda (selected)
                            (when (and selected (file-directory-p selected))
                              (let ((dir-name (file-name-nondirectory selected)))
                                ;; Switch to or create perspective
                                (my/persp-switch-or-create dir-name)
                                ;; Set default directory and open dired
                                (setq default-directory selected)
                                (dired selected)
                                (message "Perspective: %s" dir-name))))
                          nil
                          nil)))))

(provide 'my-fzf)
;;; fzf.el ends here