;;; my-navigation.el --- Navigation and directory selection with consult -*- lexical-binding: t -*-

;;; Commentary:
;; Navigation functions using consult for fuzzy directory and file selection.
;; Replaces FZF-based navigation with native Emacs consult framework.

;;==============================================================================
;; HELPER FUNCTION (doesn't depend on consult)
;;==============================================================================

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

;;==============================================================================
;; CONSULT-BASED NAVIGATION FUNCTIONS
;;==============================================================================

;; Define functions after consult is loaded
(with-eval-after-load 'consult
  
   ;; Main navigation function: Directory selection with perspective management
  ;;;###autoload
  (defun my/consult-sessionizer ()
    "Consult-based directory selection with perspective workspace management.
Switches to perspective named after directory and opens dired."
    (interactive)
    (let ((candidates (my/get-directory-candidates)))
      (when candidates
        (let ((selected (consult--read candidates
                                       :prompt "Select directory: "
                                       :require-match t
                                       :sort nil
                                       :category 'file
                                       :history 'my/directory-history)))
          (when (and selected (file-directory-p selected))
            (let ((dir-name (file-name-nondirectory selected)))
              ;; Switch to or create perspective
              (my/persp-switch-or-create dir-name)
              ;; Set default directory and open dired
              (setq default-directory selected)
              (dired selected)
              (message "Perspective: %s" dir-name)))))))

   ;; Simple directory selection (without perspective management)
  ;;;###autoload
  (defun my/consult-project-dirs ()
    "Consult-based directory selection from common project directories."
    (interactive)
    (let ((candidates (my/get-directory-candidates)))
      (when candidates
        (let ((selected (consult--read candidates
                                       :prompt "Select directory: "
                                       :require-match t
                                       :sort nil
                                       :category 'file
                                       :history 'my/directory-history)))
          (when selected
            (dired selected))))))

   ;; Enhanced consult-find with better preview and filtering
  ;;;###autoload
  (defun my/consult-find-enhanced ()
    "Enhanced consult-find with improved preview and filtering."
    (interactive)
    (consult-find (my/current-dir)))

   ;; Directory-based consult-ripgrep
  ;;;###autoload
  (defun my/consult-ripgrep-dir ()
    "Run consult-ripgrep starting from selected directory."
    (interactive)
    (let ((candidates (my/get-directory-candidates)))
      (when candidates
        (let ((selected (consult--read candidates
                                       :prompt "Search in directory: "
                                       :require-match t
                                       :sort nil
                                       :category 'file
                                       :history 'my/directory-history)))
          (when selected
            (let ((default-directory selected))
              (call-interactively #'consult-ripgrep)))))))

   ;; Quick file search in current directory
  ;;;###autoload
  (defun my/consult-find-current ()
    "Quick file search in current directory with consult-find."
    (interactive)
    (consult-find (my/current-dir))))

;;==============================================================================
;; INTEGRATION WITH EXISTING WORKFLOW
;;==============================================================================

;; Note: These functions replace the FZF-based functions:
;; - my/consult-sessionizer replaces my/emacs-sessionizer
;; - my/consult-project-dirs replaces my/fzf-project-dirs
;; - my/consult-find-enhanced replaces fzf-directory
;; - consult-ripgrep already exists for grep functionality

(provide 'my-navigation)
;;; my-navigation.el ends here