;;; my-navigation.el --- Navigation and directory selection with consult -*- lexical-binding: t -*-

;;; Commentary:
;; Navigation functions using consult for fuzzy directory and file selection.
;; Replaces FZF-based navigation with native Emacs consult framework.

;;==============================================================================
;; CUSTOMIZATION
;;==============================================================================

(defcustom my/directory-roots
  (list (expand-file-name "~/files")
        (expand-file-name "~/projects")
        (expand-file-name "~")
        (expand-file-name "~/.config")
        (expand-file-name "~/.local")
        (expand-file-name "/home"))
  "List of root directories to search for subdirectories."
  :type '(repeat directory)
  :group 'my-navigation)

;;==============================================================================
;; HELPER FUNCTION (doesn't depend on consult)
;;==============================================================================

(defun my/get-directory-candidates ()
  "Return list of subdirectory paths from common directories."
  (let ((roots my/directory-roots)
        candidates)
    (dolist (root roots candidates)
      (when (file-directory-p root)
        ;; Include the root directory itself
        (push root candidates)
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

;; Main navigation function: Directory selection
;;;###autoload
(defun my/consult-sessionizer ()
  "Consult-based directory selection.
Opens selected directory in dired."
  (interactive)
  (require 'consult)
  (let ((candidates (my/get-directory-candidates)))
    (if candidates
        (condition-case err
            (let ((selected (consult--read candidates
                                           :prompt "Select directory: "
                                           :require-match t
                                           :sort nil
                                           :category 'file
                                           :history 'my/directory-history)))
              (when (and selected (file-directory-p selected))
                 ;; Set default directory and open dired
                 (setq default-directory selected)
                 (dired selected)
                  (message "Opened directory: %s" selected)))
          (error
           (message "Error in consult-sessionizer: %s" (error-message-string err))
           nil))
      ;; No candidates: fallback to dired in default-directory
      (dired default-directory)
      (message "Opened current directory"))))

;; Simple directory selection (without perspective management)
;;;###autoload
(defun my/consult-project-dirs ()
  "Consult-based directory selection from common project directories."
  (interactive)
  (require 'consult)
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



;;==============================================================================
;; INTEGRATION WITH EXISTING WORKFLOW
;;==============================================================================

;; Note: These functions replace the FZF-based functions:
;; - my/consult-sessionizer replaces my/emacs-sessionizer
;; - my/consult-project-dirs replaces my/fzf-project-dirs
;; - consult-ripgrep already exists for grep functionality

(provide 'my-navigation)
;;; my-navigation.el ends here