;;; keybindings.el --- Tmux-style keybindings configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Keybindings using general.el with C-SPC leader (works in all contexts)

(use-package general
  :ensure t
  :demand t
  :config
  (setq general-warn-on-override nil)
  
  ;; Unbind C-SPC from set-mark-command, rebind to C-@
  (general-unbind :states '(normal insert visual emacs motion operator replace) "C-SPC")
  (general-unbind "C-SPC")
  (general-define-key
   :states '(normal insert visual emacs motion operator replace)
   :keymaps 'global
   "C-@" 'set-mark-command)
  
  ;; Single leader definition (works for evil AND non-evil contexts via 'emacs state)
  (general-create-definer my-leader-def
    :states '(normal insert visual emacs motion operator replace)
    :keymaps 'global
    :prefix "C-SPC")
  
  (my-leader-def
    ;; Files
    "f"   '(:ignore t :which-key "files")
    "ff"  '(find-file :which-key "find file")
    "fg"  '(my/consult-find-current-dir :which-key "fuzzy find files")
    "fs"  '(save-buffer :which-key "save file")
    "fr"  '(recentf-open-files :which-key "recent files")
    "fd"  '(my/consult-project-dirs :which-key "find directory")
    "fz"  '(my/consult-find-enhanced :which-key "consult file search")
    "f/"  '(my/consult-ripgrep-dir :which-key "ripgrep directory")
    
    ;; Search
    "/"   '(my/consult-ripgrep-current-dir :which-key "search in current dir")
    "R"   '(my/consult-ripgrep-toggle-regex :which-key "toggle regex/fixed")
    
    ;; Buffers
    "b"   '(:ignore t :which-key "buffer")
    "bs"  '(my/persp-ibuffer-full :which-key "list buffers")
    "bn"  '(next-buffer :which-key "next buffer")
    "bp"  '(previous-buffer :which-key "previous buffer")
    "bl"  '(evil-switch-to-windows-last-buffer :which-key "last buffer")
    
    ;; Single keys
    "s"   '(my/persp-switch-full :which-key "switch perspective")
    "x"   '(my/tab-line-close-current-tab :which-key "close tab")
    "c"   '(my/open-current-dir-dired :which-key "file explorer")
    
    ;; Terminal
    "t"   '(:ignore t :which-key "terminal")
    "te"  '(my/open-eshell-here :which-key "eshell")
    "tt"  '(my/open-eat-eshell-here :which-key "eat eshell")
    
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
    
    ;; Perspective
    "p"   '(:ignore t :which-key "perspective")
    "ps"  '(persp-switch :which-key "switch perspective")
    "pa"  '(my/persp-switch-or-create :which-key "switch/create")
    "pc"  '(my/persp-kill-current :which-key "kill current")
    "pr"  '(my/persp-rename-current :which-key "rename current")
    "pn"  '(persp-next :which-key "next")
    "pp"  '(persp-prev :which-key "previous")
    "pl"  '(persp-switch-last :which-key "last")
    "pq"  '(persp-switch-quick :which-key "quick switch")
    "pi"  '(my/persp-list :which-key "list")
    "pS"  '(persp-state-save :which-key "save")
    "pL"  '(persp-state-load :which-key "load")
    
    ;; Help
    "h"   '(:ignore t :which-key "help")
    "hf"  '(helpful-callable :which-key "function")
    "hv"  '(helpful-variable :which-key "variable")
    "hk"  '(helpful-key :which-key "key")
    
    ;; Git
    "g"   '(:ignore t :which-key "git")
    "gs"  '(my/magit-status-current-dir :which-key "status")
    "gd"  '(my/magit-dispatch :which-key "dispatch")
    "gb"  '(my/magit-blame-current-file :which-key "blame")
    "gl"  '(my/magit-log-current-file :which-key "log")
    "gc"  '(my/magic-commit :which-key "commit")
    "gp"  '(my/magit-push-current :which-key "push")
    "gP"  '(my/magit-pull-current :which-key "pull")
    "gg"  '(magit-status :which-key "magit status")
    "gS"  '(my/magit-stash :which-key "stash")
    "gB"  '(my/magit-branch :which-key "branch")
    "gm"  '(my/magit-merge :which-key "merge")
    "gr"  '(my/magit-rebase :which-key "rebase")
    "gf"  '(my/magit-fetch :which-key "fetch")
    "gR"  '(my/magit-remote :which-key "remote")
    
    ;; Quit
    "q"   '(:ignore t :which-key "quit")
    "qq"  '(save-buffers-kill-terminal :which-key "quit emacs"))

  ;; Numbered tab jumping (1-9)
  (dotimes (i 9)
    (let ((n (1+ i)))
      (general-define-key
       :states '(normal insert visual emacs motion operator replace)
       :keymaps 'global
       (concat "C-SPC " (number-to-string n))
       `(lambda ()
          (interactive)
          (let* ((tabs (funcall tab-line-tabs-function))
                 (buf (nth ,(1- n) tabs)))
            (when buf (switch-to-buffer buf)))))))

  ;; Font size with line number scaling
  (defvar-local my/line-number-scale-cookies nil)
  
  (defun my/update-line-number-scale ()
    (dolist (cookie my/line-number-scale-cookies)
      (face-remap-remove-relative cookie))
    (setq my/line-number-scale-cookies nil)
    (let ((amount (text-scale-mode-amount)))
      (when (not (zerop amount))
        (push (face-remap-add-relative 'line-number 
                :height (expt text-scale-mode-step amount))
              my/line-number-scale-cookies)
        (push (face-remap-add-relative 'line-number-current-line 
                :height (expt text-scale-mode-step amount))
              my/line-number-scale-cookies))))
  
  (advice-add 'text-scale-increase :after #'my/update-line-number-scale)
  (advice-add 'text-scale-decrease :after #'my/update-line-number-scale)
  (advice-add 'text-scale-adjust :after #'my/update-line-number-scale)
  
  (general-define-key
   :states '(normal insert visual emacs motion operator replace)
   :keymaps 'global
   "C-=" 'text-scale-increase
   "C--" 'text-scale-decrease
   "C-0" 'text-scale-adjust
   "C-n" 'next-line
   "C-p" 'previous-line
   ;; Global copy/paste (works everywhere including terminals)
   "C-S-c" 'clipboard-kill-ring-save
   "C-S-v" 'clipboard-yank)

  ;; Global shortcuts (no leader)
  (general-define-key
   :states '(normal insert visual emacs motion operator replace)
   :keymaps 'global
   "C-f"   'my/consult-sessionizer
   "C-S-n" 'my/create-new-project)
  
  (when (fboundp 'my/ssh-sessionizer)
    (general-define-key
     :states '(normal insert visual emacs motion operator replace)
     :keymaps 'global
     "C-S-f" 'my/ssh-sessionizer))
  
  (when (fboundp 'my/open-github-pr)
    (general-define-key
     :states '(normal insert visual emacs motion operator replace)
     :keymaps 'global
     "C-S-p" 'my/open-github-pr)))

;; Evil normal mode shortcuts
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "-") #'my/open-current-dir-dired))

;; Tab navigation functions
(require 'cl-lib)
(defun my/tab-line-next-tab ()
  "Switch to next tab."
  (interactive)
  (let* ((tabs (funcall tab-line-tabs-function))
         (current-index (cl-position (current-buffer) tabs :test #'eq))
         (next-index (if current-index
                         (mod (1+ current-index) (length tabs))
                       0)))
    (when (and tabs (> (length tabs) 0))
      (switch-to-buffer (nth next-index tabs)))))

(defun my/tab-line-prev-tab ()
  "Switch to previous tab."
  (interactive)
  (let* ((tabs (funcall tab-line-tabs-function))
         (current-index (cl-position (current-buffer) tabs :test #'eq))
         (prev-index (if current-index
                         (mod (1- current-index) (length tabs))
                       (1- (length tabs)))))
    (when (and tabs (> (length tabs) 0))
      (switch-to-buffer (nth prev-index tabs)))))

;; Tab navigation bindings
(general-define-key
 :states '(normal insert visual emacs motion operator replace)
 :keymaps 'global
 "C-SPC <right>" 'my/tab-line-next-tab
 "C-SPC <left>"  'my/tab-line-prev-tab
 "M-<right>"     'my/tab-line-next-tab
 "M-<left>"      'my/tab-line-prev-tab)

(provide 'keybindings)
;;; keybindings.el ends here
