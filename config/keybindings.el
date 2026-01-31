;;; keybindings.el --- Tmux-style keybindings configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Keybindings using general.el with C-x SPC leader (works in all contexts)

(use-package general
  :ensure t
  :demand t
  :config
  (setq general-warn-on-override nil)
  
   ;; Unbind C-SPC from set-mark-command, rebind to C-@
   (general-unbind :states '(normal insert visual emacs motion operator replace) "C-SPC")
   (general-unbind "C-SPC")
   ;; Unbind C-x SPC from rectangle-mark-mode (bound to C-S-@)
   (general-unbind "C-x SPC")
   ;; Remove default binding in ctl-x-map
   (define-key ctl-x-map (kbd "SPC") nil)
  (general-define-key
   :states '(normal insert visual emacs motion operator replace)
   :keymaps 'global
   "C-@" 'set-mark-command)
  
  ;; Explicitly bind C-SPC as prefix key in global keymap (ensures it works everywhere)
  (general-define-key
   :states nil  ; No state restrictions
   :keymaps 'global
   "C-SPC" nil)  ; Define as prefix key
  
   ;; Global leader definition (no state restrictions, works everywhere)
   (general-create-definer my-leader-def
     :states nil  ; No state restrictions - works in all contexts
     :keymaps 'global
     :prefix "C-x SPC")
  
  (my-leader-def
    ;; Files
    "f"   '(:ignore t :which-key "files")
       "ff"  '(find-file :which-key "find file")
     "fs"  '(save-buffer :which-key "save file")
    "fr"  '(recentf-open-files :which-key "recent files")
     "fd"  '(my/consult-project-dirs :which-key "find directory")

     ;; Buffers
    "b"   '(:ignore t :which-key "buffer")
    "bs"  '(ibuffer :which-key "list buffers")
    "bn"  '(next-buffer :which-key "next buffer")
    "bp"  '(previous-buffer :which-key "previous buffer")
    "bl"  '(evil-switch-to-windows-last-buffer :which-key "last buffer")
    
    ;; Harpoon marking
    "m"   '(:ignore t :which-key "mark buffer")
    "m1"  '(my/harpoon-mark-buffer-1 :which-key "mark slot 1")
    "m2"  '(my/harpoon-mark-buffer-2 :which-key "mark slot 2")
    "m3"  '(my/harpoon-mark-buffer-3 :which-key "mark slot 3")
    "m4"  '(my/harpoon-mark-buffer-4 :which-key "mark slot 4")
    "m5"  '(my/harpoon-mark-buffer-5 :which-key "mark slot 5")
    "m6"  '(my/harpoon-mark-buffer-6 :which-key "mark slot 6")
    "mc"  '(my/harpoon-clear-slot :which-key "clear slot")
    "ml"  '(my/harpoon-list-buffers :which-key "list marked buffers")
    
     ;; Single keys
     "c"   '(my/open-current-dir-dired :which-key "file explorer")
     
     ;; Editing
     "e"   '(:ignore t :which-key "editing")
     "ed"  '(my/duplicate-line :which-key "duplicate line/region")
     "eu"  '(my/unfill-paragraph :which-key "unfill paragraph")
    
    ;; Terminal
    "t"   '(:ignore t :which-key "terminal")
    "te"  '(my/open-eshell-here :which-key "eshell")
    "tt"  '(my/open-eat-eshell-here :which-key "eat eshell")
    "tv"  '(my/open-vterm-here :which-key "vterm")
    "ta"  '(my/open-ansi-term-here :which-key "ansi term")
    
    ;; Windows
    "w"   '(:ignore t :which-key "windows")
    "ws"  '(split-window-below :which-key "split horizontal")
    "wv"  '(split-window-right :which-key "split vertical")
    "wd"  '(delete-window :which-key "delete window")
    "wo"  '(delete-other-windows :which-key "only this window")
    "wh"  '(windmove-left :which-key "left")
    "wj"  '(windmove-down :which-key "down")
    "wk"  '(windmove-up :which-key "up")
    "wl"  '(windmove-right :which-key "right")
    "ww"  '(other-window :which-key "next window")
    "w\"" '(split-window-below :which-key "split horizontal (tmux)")
    "w%"  '(split-window-right :which-key "split vertical (tmux)")
    "wq"  '(delete-window :which-key "close pane (tmux)")
    "wz"  '(delete-other-windows :which-key "zoom pane (tmux)")
    

    
    ;; Help
    "h"   '(:ignore t :which-key "help")
    "hf"  '(helpful-callable :which-key "function")
    "hv"  '(helpful-variable :which-key "variable")
    "hk"  '(helpful-key :which-key "key")
    
    ;; Git
    "g"   '(:ignore t :which-key "git")
    "gs"  '(my/magit-status-current-dir :which-key "status")
    "gd"  '(my/magit-dispatch :which-key "dispatch")

     
     ;; Notes
     "n"   '(:ignore t :which-key "notes")
     "nf"  '(org-roam-node-find :which-key "find node")
     "ni"  '(org-roam-node-insert :which-key "insert node")
     "nc"  '(org-roam-capture :which-key "capture note")
     "nl"  '(org-roam-buffer-toggle :which-key "toggle backlinks")
     "nd"  '(org-roam-dailies-capture-today :which-key "daily note")
     "nD"  '(org-roam-dailies-goto-today :which-key "goto today")
       "ns"  '(my/org-roam-search-notes :which-key "search notes")
       "no"  '(my/org-roam-open-directory :which-key "open notes directory")
       ;; Workout templates
       "nw1" '(my/org-roam-capture-workout-1 :which-key "workout day 1")
       "nw2" '(my/org-roam-capture-workout-2 :which-key "workout day 2")
       "nw3" '(my/org-roam-capture-workout-3 :which-key "workout day 3")
       "nw4" '(my/org-roam-capture-workout-4 :which-key "workout day 4")
       "nw5" '(my/org-roam-capture-workout-5 :which-key "workout day 5")
 



      ;; Quit
     "q"   '(:ignore t :which-key "quit")
      "qq"  '(save-buffers-kill-terminal :which-key "quit emacs"))

;; Generated helper functions for each slot
(dotimes (i 6)
  (let ((slot (1+ i)))
    (defalias (intern (format "my/harpoon-mark-buffer-%d" slot))
      `(lambda () (interactive) (my/harpoon-mark-buffer ,slot)))))

      ;; Harpoon jump to marked buffers (C-x SPC j 1-6) - global bindings
     (dotimes (i 6)
       (let ((n (1+ i)))
         (general-define-key
          :states nil  ; Global - no state restrictions
          :keymaps 'global
          (concat "C-x SPC j " (number-to-string n))
          `(lambda ()
              (interactive)
               (my/harpoon-jump-to-buffer ,n)))))
   
     ;; Harpoon previous buffer (C-x SPC j 0) - global binding
     (general-define-key
      :states nil  ; Global - no state restrictions
      :keymaps 'global
      "C-x SPC j 0" 'my/harpoon-jump-to-previous-buffer)
  
    (general-define-key
     :states nil  ; Global - no state restrictions
     :keymaps 'global
     "C-=" 'text-scale-increase
     "C--" 'text-scale-decrease
     "C-0" 'text-scale-adjust
     "C-n" 'next-line
     "C-p" 'previous-line
     ;; Global copy/paste (works everywhere including terminals)
     "C-S-c" 'clipboard-kill-ring-save
     "C-S-v" 'clipboard-yank
     ;; Text editing utilities
     "C-S-d" 'my/duplicate-line
     "M-Q"   'my/unfill-paragraph)

  ;; Global shortcuts (no leader)
   (general-define-key
    :states nil  ; Global - no state restrictions
    :keymaps 'global
    "C-f"   'my/consult-sessionizer
    "C-S-n" 'my/create-new-project
    "C-S-@" 'rectangle-mark-mode)
  
  (when (fboundp 'my/ssh-sessionizer)
     (general-define-key
      :states nil  ; Global - no state restrictions
      :keymaps 'global
      "C-S-f" 'my/ssh-sessionizer))
  
  (when (fboundp 'my/open-github-pr)
     (general-define-key
      :states nil  ; Global - no state restrictions
      :keymaps 'global
      "C-S-p" 'my/open-github-pr)))

;; Evil normal mode shortcuts
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "-") #'my/open-current-dir-dired)
  (general-define-key
   :states '(normal insert visual emacs motion operator replace)
   :keymaps 'global
   "C-f" 'my/consult-sessionizer))

;; Harpoon buffer marking system
(defvar my/harpoon-buffers (make-vector 6 nil)
  "Vector of 6 marked buffers for quick access.")

(defvar my/harpoon-previous-buffer nil
  "Previous buffer before last buffer switch.")

(defun my/harpoon--record-before-switch (buffer &rest _)
  "Record current buffer before switching to BUFFER."
  (unless (eq (current-buffer) buffer)
    (setq my/harpoon-previous-buffer (current-buffer))))

(advice-add 'switch-to-buffer :before #'my/harpoon--record-before-switch)

(defun my/harpoon-mark-buffer (slot)
  "Mark current buffer in SLOT (1-6)."
  (interactive "nMark buffer in slot (1-6): ")
  (when (and (>= slot 1) (<= slot 6))
    (aset my/harpoon-buffers (1- slot) (current-buffer))
    (message "Marked buffer %s in slot %d" (buffer-name) slot)))

(defun my/harpoon-jump-to-buffer (slot)
  "Jump to buffer marked in SLOT (1-6)."
  (interactive "nJump to slot (1-6): ")
  (when (and (>= slot 1) (<= slot 6))
    (let ((buf (aref my/harpoon-buffers (1- slot))))
      (if (buffer-live-p buf)
           (switch-to-buffer buf)
        (message "Slot %d is empty or buffer no longer exists" slot)))))

(defun my/harpoon-clear-slot (slot)
  "Clear marked buffer in SLOT (1-6)."
  (interactive "nClear slot (1-6): ")
  (when (and (>= slot 1) (<= slot 6))
    (aset my/harpoon-buffers (1- slot) nil)
    (message "Cleared slot %d" slot)))

(defun my/harpoon-list-buffers ()
  "List all marked buffers."
  (interactive)
  (message "Harpoon buffers:")
  (dotimes (i 6)
    (let ((buf (aref my/harpoon-buffers i)))
      (when (buffer-live-p buf)
        (message "  %d: %s" (1+ i) (buffer-name buf))))))



(defun my/harpoon-jump-to-previous-buffer ()
  "Jump to previous buffer (before last harpoon jump)."
  (interactive)
  (if (and my/harpoon-previous-buffer (buffer-live-p my/harpoon-previous-buffer))
      (switch-to-buffer my/harpoon-previous-buffer)
    (message "No previous buffer recorded")))

(provide 'keybindings)
;;; keybindings.el ends here
