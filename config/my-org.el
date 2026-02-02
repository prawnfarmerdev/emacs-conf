;;; my-org.el --- Org-mode and org-roam configuration for Zettelkasten -*- lexical-binding: t -*-

;;; Commentary:
;; Org-mode and org-roam v2 configuration for Zettelkasten note-taking system.

;;==============================================================================
;; ORG-ROAM V2
;;==============================================================================

(use-package org-roam
  :ensure t
  :defer t
  :init
  ;; Set the directory for org-roam notes
  (setq org-roam-directory (expand-file-name "~/notes"))
  ;; Ensure directory exists
  (unless (file-directory-p org-roam-directory)
    (make-directory org-roam-directory t))
  ;; Enable v2
  (setq org-roam-v2-ack t)
  :custom
  ;; Database location
  (org-roam-db-location (expand-file-name "org-roam.db" org-roam-directory))
  ;; Use completion for node insertion
  (org-roam-completion-everywhere t)
  ;; Use consult for node finding
  (org-roam-node-display-template "${title:*} ${tags:10}")
  :config
  (org-roam-setup))

;;==============================================================================
;; ORG-ROAM-UI (optional)
;;==============================================================================

;; Uncomment to enable org-roam-ui (requires node.js)
;; (use-package org-roam-ui
;;   :ensure t
;;   :after org-roam
;;   :config
;;   (setq org-roam-ui-sync-theme t
;;         org-roam-ui-follow t
;;         org-roam-ui-update-on-save t
;;         org-roam-ui-open-on-start nil))

;;==============================================================================
;; ORG-ROAM CAPTURE TEMPLATES
;;==============================================================================

(setq org-roam-capture-templates
      '(("d" "default" plain
         "%?"
         :target (file+head "${slug}.org"
                  "#+title: ${title}\n#+date: %<%Y-%m-%d>\n#+filetags: \n\n")
         :unnarrowed t)
        ("p" "project" plain
         "* Goals\n%?\n* Tasks\n** TODO\n** TODO\n** TODO\n"
         :target (file+head "${slug}.org"
                  "#+title: ${title}\n#+date: %<%Y-%m-%d>\n#+filetags: project\n\n")
         :unnarrowed t)
        ("l" "literature" plain
         "* Summary\n%?\n* Quotes\n\n* References\n"
         :target (file+head "${slug}.org"
                  "#+title: ${title}\n#+date: %<%Y-%m-%d>\n#+filetags: literature\n\n")
         :unnarrowed t)
        ("w1" "workout day 1" plain
         "* Workout Overview\n** Focus\n%?\n** Duration\n~60 minutes\n\n** Equipment\n- Kettlebell\n- Timer\n\n* Warm-up (5-10 minutes)\n- [ ] \n- [ ] \n- [ ] \n\n* Main Workout\n** Exercise 1: \n- Sets: \n- Reps: \n** Exercise 2: \n- Sets: \n- Reps: \n** Exercise 3: \n- Sets: \n- Reps: \n\n* Cool-down (5 minutes)\n- [ ] \n- [ ] \n\n* Notes\n- \n"
         :target (file+head "${slug}.org"
                  "#+title: ${title}\n#+date: %<%Y-%m-%d>\n#+filetags: kettlebell workout\n\n")
         :unnarrowed t)
        ("w2" "workout day 2" plain
         "* Workout Overview\n** Focus\n%?\n** Duration\n~60 minutes\n\n** Equipment\n- Kettlebell\n- Timer\n\n* Warm-up (5-10 minutes)\n- [ ] \n- [ ] \n- [ ] \n\n* Main Workout\n** Exercise 1: \n- Sets: \n- Reps: \n** Exercise 2: \n- Sets: \n- Reps: \n** Exercise 3: \n- Sets: \n- Reps: \n\n* Cool-down (5 minutes)\n- [ ] \n- [ ] \n\n* Notes\n- \n"
         :target (file+head "${slug}.org"
                  "#+title: ${title}\n#+date: %<%Y-%m-%d>\n#+filetags: kettlebell workout\n\n")
         :unnarrowed t)
        ("w3" "workout day 3" plain
         "* Workout Overview\n** Focus\n%?\n** Duration\n~60 minutes\n\n** Equipment\n- Kettlebell\n- Timer\n\n* Warm-up (5-10 minutes)\n- [ ] \n- [ ] \n- [ ] \n\n* Main Workout\n** Exercise 1: \n- Sets: \n- Reps: \n** Exercise 2: \n- Sets: \n- Reps: \n** Exercise 3: \n- Sets: \n- Reps: \n\n* Cool-down (5 minutes)\n- [ ] \n- [ ] \n\n* Notes\n- \n"
         :target (file+head "${slug}.org"
                  "#+title: ${title}\n#+date: %<%Y-%m-%d>\n#+filetags: kettlebell workout\n\n")
         :unnarrowed t)
        ("w4" "workout day 4" plain
         "* Workout Overview\n** Focus\n%?\n** Duration\n~60 minutes\n\n** Equipment\n- Kettlebell\n- Timer\n\n* Warm-up (5-10 minutes)\n- [ ] \n- [ ] \n- [ ] \n\n* Main Workout\n** Exercise 1: \n- Sets: \n- Reps: \n** Exercise 2: \n- Sets: \n- Reps: \n** Exercise 3: \n- Sets: \n- Reps: \n\n* Cool-down (5 minutes)\n- [ ] \n- [ ] \n\n* Notes\n- \n"
         :target (file+head "${slug}.org"
                  "#+title: ${title}\n#+date: %<%Y-%m-%d>\n#+filetags: kettlebell workout\n\n")
         :unnarrowed t)
        ("w5" "workout day 5" plain
         "* Workout Overview\n** Focus\n%?\n** Duration\n~60 minutes\n\n** Equipment\n- Kettlebell\n- Timer\n\n* Warm-up (5-10 minutes)\n- [ ] \n- [ ] \n- [ ] \n\n* Main Workout\n** Exercise 1: \n- Sets: \n- Reps: \n** Exercise 2: \n- Sets: \n- Reps: \n** Exercise 3: \n- Sets: \n- Reps: \n\n* Cool-down (5 minutes)\n- [ ] \n- [ ] \n\n* Notes\n- \n"
         :target (file+head "${slug}.org"
                  "#+title: ${title}\n#+date: %<%Y-%m-%d>\n#+filetags: kettlebell workout\n\n")
         :unnarrowed t)))

;;==============================================================================
;; ORG-ROAM DAILY NOTES
;;==============================================================================

(setq org-roam-dailies-directory "daily/")
(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :target (file+head "%<%Y-%m-%d>.org"
                  "#+title: %<%Y-%m-%d>\n\n"))))

;;==============================================================================
;; KEYBINDINGS
;;==============================================================================

;; Global prefix for org-roam commands (C-c n)
(define-prefix-command 'my/org-roam-prefix-map)
(global-set-key (kbd "C-c n") 'my/org-roam-prefix-map)

;; Node operations
(define-key my/org-roam-prefix-map (kbd "f") 'org-roam-node-find)
(define-key my/org-roam-prefix-map (kbd "i") 'org-roam-node-insert)
(define-key my/org-roam-prefix-map (kbd "c") 'org-roam-capture)
(define-key my/org-roam-prefix-map (kbd "l") 'org-roam-buffer-toggle)
(define-key my/org-roam-prefix-map (kbd "t") 'org-roam-tag-add)
(define-key my/org-roam-prefix-map (kbd "r") 'org-roam-ref-add)

;; Daily notes
(define-key my/org-roam-prefix-map (kbd "d") 'org-roam-dailies-capture-today)
(define-key my/org-roam-prefix-map (kbd "D") 'org-roam-dailies-goto-today)

;; Workout templates
(define-key my/org-roam-prefix-map (kbd "w 1") 'my/org-roam-capture-workout-1)
(define-key my/org-roam-prefix-map (kbd "w 2") 'my/org-roam-capture-workout-2)
(define-key my/org-roam-prefix-map (kbd "w 3") 'my/org-roam-capture-workout-3)
(define-key my/org-roam-prefix-map (kbd "w 4") 'my/org-roam-capture-workout-4)
(define-key my/org-roam-prefix-map (kbd "w 5") 'my/org-roam-capture-workout-5)

;; UI (if enabled)
;; (define-key my/org-roam-prefix-map (kbd "u") 'org-roam-ui-mode)

;;==============================================================================
;; ORG-MODE BASIC CONFIGURATION
;;==============================================================================

;; Enable org-mode for .org files
(require 'org)
(require 'org-tempo)
(org-tempo-setup)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; Basic org settings
(setq org-startup-indented t
      org-adapt-indentation t
      org-hide-emphasis-markers t
      org-pretty-entities t
      org-ellipsis " ⤵"
      org-hide-leading-stars t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t
      org-fontify-whole-heading-line t)

;; Enable org-id for org-roam compatibility
(require 'org-id)
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

;; Enable org-indent-mode for cleaner display (hides leading stars)
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook (lambda ()
                           (remove-hook 'completion-at-point-functions 'ispell-completion-at-point t)))

;;==============================================================================
;; ORG-BABEL CONFIGURATION
;;==============================================================================
;; Enable execution of code blocks for Python, Go, C, etc.
;; Ensure corresponding compilers/interpreter are installed: python3, go, gcc

(require 'ob)
;; Ensure language backends are loaded for completion
(unless (featurep 'ob-python)
  (require 'ob-python nil t))
(unless (featurep 'ob-go)
  (require 'ob-go nil t))
(unless (featurep 'ob-C)
  (require 'ob-C nil t))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (go . t)
   (C . t)
   (emacs-lisp . t)
   (shell . t)))


;; Disable confirmation for code block execution (set to t for safety)
(setq org-confirm-babel-evaluate nil)

;; Language completion for #+begin_src blocks
(defun my/org-language-completion-at-point ()
  "Provide completion for language names after #+begin_src."
  (when (and (looking-back "#\\+begin_src \\(.*\\)" (line-beginning-position))
             (not (string-match-p "[[:space:]]"
                                  (match-string 1))))
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (list (or (car bounds) (point))
            (or (cdr bounds) (point))
            (mapcar #'symbol-name (mapcar #'car org-babel-load-languages))
            :exclusive 'no))))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'completion-at-point-functions
                      #'my/org-language-completion-at-point nil t)))

;;==============================================================================
;; HELPER FUNCTIONS
;;==============================================================================

(defun my/org-roam-open-directory ()
  "Open org-roam directory in dired."
  (interactive)
  (dired org-roam-directory))

(defun my/org-roam-search-notes ()
  "Search org-roam notes using consult-ripgrep."
  (interactive)
  (consult-ripgrep org-roam-directory))

;; Generated workout capture functions
(dotimes (i 5)
  (let ((day (1+ i)))
    (eval `(defun ,(intern (format "my/org-roam-capture-workout-%d" day)) ()
             ,(format "Capture a workout note using template w%d." day)
             (interactive)
             (require 'org-roam)
             (org-roam-capture nil ,(format "w%d" day))))))

;; Add helper to keymap
(define-key my/org-roam-prefix-map (kbd "o") 'my/org-roam-open-directory)
(define-key my/org-roam-prefix-map (kbd "s") 'my/org-roam-search-notes)

(defun my/org-diagnostic ()
  "Display diagnostic information about org-mode configuration."
  (interactive)
  (let ((buf (get-buffer-create "*Org Diagnostic*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "=== Org-mode Diagnostic ===\n\n")
      
      (insert "1. Current Buffer:\n")
      (insert (format "   File: %s\n" (or (buffer-file-name) "No file")))
      (insert (format "   Major mode: %s\n" major-mode))
      (insert (format "   Derived from org-mode: %s\n\n" (derived-mode-p 'org-mode)))
      
      (insert "2. Org-mode Features:\n")
      (insert (format "   org feature loaded: %s\n" (featurep 'org)))
      (insert (format "   org-indent feature: %s\n\n" (featurep 'org-indent)))
      
      (insert "3. Org-mode Settings:\n")
      (insert (format "   org-hide-leading-stars: %s\n" org-hide-leading-stars))
      (insert (format "   org-startup-indented: %s\n" org-startup-indented))
      (insert (format "   org-indent-mode: %s\n" (bound-and-true-p org-indent-mode)))
      (insert (format "   org-hide-emphasis-markers: %s\n" org-hide-emphasis-markers))
      (insert (format "   org-pretty-entities: %s\n\n" org-pretty-entities))
      
      (insert "4. Font Lock:\n")
      (insert (format "   font-lock-mode: %s\n" (bound-and-true-p font-lock-mode)))
      (insert (format "   font-lock-keywords: %s\n\n" (if font-lock-keywords "Present" "Absent")))
      
      (insert "5. Quick Fixes:\n")
      (insert "   M-x org-mode - Switch to org-mode if not already\n")
      (insert "   M-x font-lock-mode - Enable syntax highlighting\n")
      (insert "   M-x org-indent-mode - Toggle indentation\n")
      (insert "   M-x org-reload - Reload org-mode\n"))
    (pop-to-buffer buf)))

;; Add diagnostic to keymap
(define-key my/org-roam-prefix-map (kbd "?") 'my/org-diagnostic)

(defun my/org-fix-display ()
  "Fix org-mode display issues in current buffer."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (org-mode))
  (font-lock-mode 1)
  (org-indent-mode 1)
  (message "Org display fixed: font-lock %s, org-indent %s"
           (if font-lock-mode "on" "off")
           (if (bound-and-true-p org-indent-mode) "on" "off")))

;; Add fix to keymap
(define-key my/org-roam-prefix-map (kbd "!") 'my/org-fix-display)

;;==============================================================================
;; INITIALIZATION
;;==============================================================================

;; Ensure org-roam directory exists and database is built
(add-hook 'emacs-startup-hook
          (lambda ()
            (when (file-directory-p org-roam-directory)
              (org-roam-db-autosync-enable))))

(provide 'my-org)
;;; my-org.el ends here