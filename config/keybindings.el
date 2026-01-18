;;; keybindings.el --- Tmux-style keybindings configuration

;;; Commentary:
;; Keybindings configuration using general.el with tmux/Vim-style bindings.
;; Note: Depends on helper functions from helpers.el and other modules.

;;==============================================================================
;; TMUX-STYLE KEYBINDINGS
;;==============================================================================

(use-package general
  :ensure t
  :demand t
  :config
  
  ;; NORMAL/VISUAL MODE (C-SPC leader) - Vim-style with Ctrl+Space
  ;; Use 'global instead of 'override for better compatibility
  ;; Unbind C-SPC from set-mark-command to use as leader
  (general-unbind "C-SPC")
  (general-define-key
   :states '(normal insert visual emacs)
   :keymaps 'global
   "C-@" 'set-mark-command)
  (general-create-definer my-leader-def
    :states '(normal insert visual emacs)
    :keymaps 'global
    :prefix "C-SPC")
  
  (my-leader-def
    ;; Files
    "f"  '(:ignore t :which-key "files")
     "ff" '(find-file :which-key "find file")
     "fg" '(my/consult-find-current-dir :which-key "fuzzy find files (consult)")
     "fs" '(save-buffer :which-key "save file")
     "fr" '(recentf-open-files :which-key "recent files")
      "fd" '(my/consult-project-dirs :which-key "find directory (consult)")
      "fz" '(my/consult-find-enhanced :which-key "consult file search")
      "f/" '(my/consult-ripgrep-dir :which-key "consult ripgrep directory")
    
    ;; Search
    "/" '(my/consult-ripgrep-current-dir :which-key "search text in current dir")
    "R" '(my/consult-ripgrep-toggle-regex :which-key "toggle regex/fixed search")
    
      ;; Buffer management (all under b prefix)
      "b"  '(:ignore t :which-key "buffer")
      "bs" '(my/persp-ibuffer-full :which-key "list buffers in current perspective (full page)")
      "bn" '(next-buffer :which-key "next buffer")
      "bp" '(previous-buffer :which-key "previous buffer")
      "bl" '(evil-switch-to-windows-last-buffer :which-key "last buffer")
     ;; Single keys
     "s" '(my/persp-switch-full :which-key "switch perspective (full screen)")
     "x" '(my/tab-line-close-current-tab :which-key "close tab / switch from read-only")
     "c" '(my/open-current-dir-dired :which-key "open file explorer (dired) in current dir")
      "t" '(my/open-vterm-here :which-key "terminal (eshell)")
     
     ;; Windows
    "w"  '(:ignore t :which-key "windows")
    "ws" '(split-window-below :which-key "split horizontal")
    "wv" '(split-window-right :which-key "split vertical")
    "wd" '(delete-window :which-key "delete window")
    "wo" '(delete-other-windows :which-key "only this window")
    "wh" '(windmove-left  :which-key "left")
    "wj" '(windmove-down  :which-key "down")
    "wk" '(windmove-up    :which-key "up")
    "wl" '(windmove-right :which-key "right")
    "ww" '(other-window   :which-key "next window")
    ;; Tmux-style alternative bindings
    "w\"" '(split-window-below :which-key "split horizontal (tmux)")
    "w%" '(split-window-right :which-key "split vertical (tmux)")
    "wq" '(delete-window :which-key "close pane (tmux)")
    "wz" '(delete-other-windows :which-key "zoom pane (tmux)")
    
    ;; Perspective
     "p"  '(:ignore t :which-key "perspective")
     "ps" '(persp-switch :which-key "switch perspective (minibuffer)")
    "pa" '(my/persp-switch-or-create :which-key "switch/create perspective")
    "pc" '(my/persp-kill-current :which-key "kill current perspective")
    "pr" '(my/persp-rename-current :which-key "rename current perspective")
    "pn" '(persp-next :which-key "next perspective")
    "pp" '(persp-prev :which-key "previous perspective")
    "pl" '(persp-switch-last :which-key "last perspective")
    "pq" '(persp-switch-quick :which-key "quick switch perspective")
    "pi" '(my/persp-list :which-key "list perspectives")
    "pS" '(persp-state-save :which-key "save perspectives")
    "pL" '(persp-state-load :which-key "load perspectives")
    

    ;; Help
    "h"  '(:ignore t :which-key "help")
    "hf" '(helpful-callable :which-key "describe function")
    "hv" '(helpful-variable :which-key "describe variable")
    "hk" '(helpful-key :which-key "describe key")
    
    ;; Git (Magit) - lazygit-like interface
    "g"  '(:ignore t :which-key "git")
    "gs" '(my/magit-status-current-dir :which-key "status (current dir)")
    "gd" '(my/magit-dispatch :which-key "dispatch (lazygit-like)")
    "gb" '(my/magit-blame-current-file :which-key "blame current file")
    "gl" '(my/magit-log-current-file :which-key "log current file")
    "gc" '(my/magic-commit :which-key "stage & commit")
    "gp" '(my/magit-push-current :which-key "push current branch")
    "gP" '(my/magit-pull-current :which-key "pull current branch")
    "gg" '(magit-status :which-key "magit status")
    "gS" '(my/magit-stash :which-key "stash")
    "gB" '(my/magit-branch :which-key "branch")
    "gm" '(my/magit-merge :which-key "merge")
    "gr" '(my/magit-rebase :which-key "rebase")
    "gf" '(my/magit-fetch :which-key "fetch")
    "gR" '(my/magit-remote :which-key "remote")
    
    ;; Quit
    "q"  '(:ignore t :which-key "quit")
    "qq" '(save-buffers-kill-terminal :which-key "quit emacs"))

  ;; Also define global prefix for non-evil buffers
  (general-create-definer my-leader-global-def
    :keymaps 'global
    :prefix "C-SPC")
  
  (my-leader-global-def
    ;; Files
    "f"  '(:ignore t :which-key "files")
     "ff" '(find-file :which-key "find file")
     "fg" '(my/consult-find-current-dir :which-key "fuzzy find files (consult)")
     "fs" '(save-buffer :which-key "save file")
     "fr" '(recentf-open-files :which-key "recent files")
      "fd" '(my/consult-project-dirs :which-key "find directory (consult)")
      "fz" '(my/consult-find-enhanced :which-key "consult file search")
      "f/" '(my/consult-ripgrep-dir :which-key "consult ripgrep directory")
    
    ;; Search
    "/" '(my/consult-ripgrep-current-dir :which-key "search text in current dir")
    "R" '(my/consult-ripgrep-toggle-regex :which-key "toggle regex/fixed search")
    
      ;; Buffer management (all under b prefix)
      "b"  '(:ignore t :which-key "buffer")
      "bs" '(my/persp-ibuffer-full :which-key "list buffers in current perspective (full page)")
      "bn" '(next-buffer :which-key "next buffer")
      "bp" '(previous-buffer :which-key "previous buffer")
      "bl" '(evil-switch-to-windows-last-buffer :which-key "last buffer")
     ;; Single keys
     "s" '(my/persp-switch-full :which-key "switch perspective (full screen)")
     "x" '(my/tab-line-close-current-tab :which-key "close tab / switch from read-only")
     "c" '(my/open-current-dir-dired :which-key "open file explorer (dired) in current dir")
      "t" '(my/open-vterm-here :which-key "terminal (eshell)")
     
     ;; Windows
    "w"  '(:ignore t :which-key "windows")
    "ws" '(split-window-below :which-key "split horizontal")
    "wv" '(split-window-right :which-key "split vertical")
    "wd" '(delete-window :which-key "delete window")
    "wo" '(delete-other-windows :which-key "only this window")
    "wh" '(windmove-left  :which-key "left")
    "wj" '(windmove-down  :which-key "down")
    "wk" '(windmove-up    :which-key "up")
    "wl" '(windmove-right :which-key "right")
    "ww" '(other-window   :which-key "next window")
    ;; Tmux-style alternative bindings
    "w\"" '(split-window-below :which-key "split horizontal (tmux)")
    "w%" '(split-window-right :which-key "split vertical (tmux)")
    "wq" '(delete-window :which-key "close pane (tmux)")
    "wz" '(delete-other-windows :which-key "zoom pane (tmux)")
    
    ;; Perspective
     "p"  '(:ignore t :which-key "perspective")
     "ps" '(persp-switch :which-key "switch perspective (minibuffer)")
    "pa" '(my/persp-switch-or-create :which-key "switch/create perspective")
    "pc" '(my/persp-kill-current :which-key "kill current perspective")
    "pr" '(my/persp-rename-current :which-key "rename current perspective")
    "pn" '(persp-next :which-key "next perspective")
    "pp" '(persp-prev :which-key "previous perspective")
    "pl" '(persp-switch-last :which-key "last perspective")
    "pq" '(persp-switch-quick :which-key "quick switch perspective")
    "pi" '(my/persp-list :which-key "list perspectives")
    "pS" '(persp-state-save :which-key "save perspectives")
    "pL" '(persp-state-load :which-key "load perspectives")
    

    ;; Help
    "h"  '(:ignore t :which-key "help")
    "hf" '(helpful-callable :which-key "describe function")
    "hv" '(helpful-variable :which-key "describe variable")
    "hk" '(helpful-key :which-key "describe key")
    
    ;; Git (Magit) - lazygit-like interface
    "g"  '(:ignore t :which-key "git")
    "gs" '(my/magit-status-current-dir :which-key "status (current dir)")
    "gd" '(my/magit-dispatch :which-key "dispatch (lazygit-like)")
    "gb" '(my/magit-blame-current-file :which-key "blame current file")
    "gl" '(my/magit-log-current-file :which-key "log current file")
    "gc" '(my/magic-commit :which-key "stage & commit")
    "gp" '(my/magit-push-current :which-key "push current branch")
    "gP" '(my/magit-pull-current :which-key "pull current branch")
    "gg" '(magit-status :which-key "magit status")
    "gS" '(my/magit-stash :which-key "stash")
    "gB" '(my/magit-branch :which-key "branch")
    "gm" '(my/magit-merge :which-key "merge")
    "gr" '(my/magit-rebase :which-key "rebase")
    "gf" '(my/magit-fetch :which-key "fetch")
    "gR" '(my/magit-remote :which-key "remote")
    
    ;; Quit
    "q"  '(:ignore t :which-key "quit")
    "qq" '(save-buffers-kill-terminal :which-key "quit emacs"))

;; REMOVED: TMUX-STYLE BINDINGS (C-b prefix) - moved to C-SPC

  ;; Numbered tab jumping (C-SPC 1-9) - evil states
  (dotimes (i 9)
    (let ((n (1+ i)))
      (general-define-key
       :states '(normal insert visual emacs)
       :keymaps 'global
       (concat "C-SPC " (number-to-string n))
       `(lambda ()
          (interactive)
          (let* ((tabs (funcall tab-line-tabs-function))
                 (buf (nth ,(1- n) tabs)))
            (when buf (switch-to-buffer buf)))))))
  
  ;; Numbered tab jumping (C-SPC 1-9) - global (non-evil)
  (dotimes (i 9)
    (let ((n (1+ i)))
      (general-define-key
       :keymaps 'global
       (concat "C-SPC " (number-to-string n))
       `(lambda ()
          (interactive)
          (let* ((tabs (funcall tab-line-tabs-function))
                 (buf (nth ,(1- n) tabs)))
            (when buf (switch-to-buffer buf)))))))

   ;; Font size controls (work in all modes)
  (defvar-local my/line-number-scale-cookies nil
    "Cookies for line number face remapping.")
  
  (defun my/update-line-number-scale ()
    "Update line number scaling to match text scale."
    (dolist (cookie my/line-number-scale-cookies)
      (face-remap-remove-relative cookie))
    (setq my/line-number-scale-cookies nil)
    (let ((amount (text-scale-mode-amount)))
      (when (not (zerop amount))
        (push (face-remap-add-relative 'line-number :height (expt text-scale-mode-step amount))
              my/line-number-scale-cookies)
        (push (face-remap-add-relative 'line-number-current-line :height (expt text-scale-mode-step amount))
              my/line-number-scale-cookies))))
  
  (defun my/text-scale-update-line-numbers (&rest _)
    "Update line number scaling after text scale changes."
    (my/update-line-number-scale))
  
  (advice-add 'text-scale-increase :after #'my/text-scale-update-line-numbers)
  (advice-add 'text-scale-decrease :after #'my/text-scale-update-line-numbers)
  (advice-add 'text-scale-adjust :after #'my/text-scale-update-line-numbers)
  
  ;; Font size controls (work in all modes) - evil states
  (general-define-key
   :states '(normal insert visual emacs)
   :keymaps 'global
   "C-=" 'text-scale-increase
   "C--" 'text-scale-decrease
   "C-0" 'text-scale-adjust)

  ;; C-n/C-p for movement - evil states
  (general-define-key
   :states '(normal insert visual emacs)
   :keymaps 'global
   "C-n" 'next-line
   "C-p" 'previous-line)

  ;; Font size controls (work in all modes) - global
  (general-define-key
   :keymaps 'global
   "C-=" 'text-scale-increase
   "C--" 'text-scale-decrease
   "C-0" 'text-scale-adjust)

  ;; C-n/C-p for movement - global
  (general-define-key
   :keymaps 'global
   "C-n" 'next-line
   "C-p" 'previous-line)

    ;; C-f for tmux sessionizer (global) - evil states
    (general-define-key
     :states '(normal insert visual emacs)
     :keymaps 'global
     "C-f" 'my/consult-sessionizer)
    
    ;; C-S-f for SSH sessionizer (global) - evil states  
    (general-define-key
     :states '(normal insert visual emacs)
     :keymaps 'global
     "C-S-f" 'my/ssh-sessionizer)
    
    ;; C-S-p for GitHub PR (global) - evil states
    (general-define-key
     :states '(normal insert visual emacs)
     :keymaps 'global
     "C-S-p" 'my/open-github-pr)
    
    ;; C-S-n for new project (global) - evil states
    (general-define-key
     :states '(normal insert visual emacs)
     :keymaps 'global
     "C-S-n" 'my/create-new-project)
    
    ;; C-f for tmux sessionizer (global) - global
    (general-define-key
     :keymaps 'global
     "C-f" 'my/consult-sessionizer)
    
    ;; C-S-f for SSH sessionizer (global) - global
    (general-define-key
     :keymaps 'global
     "C-S-f" 'my/ssh-sessionizer)
    
    ;; C-S-p for GitHub PR (global) - global
    (general-define-key
     :keymaps 'global
     "C-S-p" 'my/open-github-pr)
    
     ;; C-S-n for new project (global) - global
     (general-define-key
      :keymaps 'global
      "C-S-n" 'my/create-new-project))

 ;; Quick access bindings in normal mode (no prefix)
 (with-eval-after-load 'evil
   (define-key evil-normal-state-map (kbd "-") #'my/open-current-dir-dired)
   (define-key evil-normal-state-map (kbd "t") #'my/open-vterm-here))

(provide 'keybindings)
;;; keybindings.el ends here
