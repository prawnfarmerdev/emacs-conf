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
      ;; Editing
      "e"   '(:ignore t :which-key "editing")
      "ed"  '(my/duplicate-line :which-key "duplicate line/region")
      "eu"  '(my/unfill-paragraph :which-key "unfill paragraph")
     
     ;; Help
     "h"   '(:ignore t :which-key "help")
     "hf"  '(helpful-callable :which-key "function")
     "hv"  '(helpful-variable :which-key "variable")
     "hk"  '(helpful-key :which-key "key")
     
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

;;==============================================================================
;; C-X PREFIX BINDINGS
;;==============================================================================

;; C-x f prefix for file operations (empty - file searches now under M-s f/s/v/x)
(defvar my-ctrl-x-f-map (make-sparse-keymap)
  "Keymap for C-x f prefix.")
(define-key ctl-x-map (kbd "f") my-ctrl-x-f-map)

;; C-x t prefix for terminal
(defvar my-ctrl-x-t-map (make-sparse-keymap)
  "Keymap for C-x t prefix.")
(define-key ctl-x-map (kbd "t") my-ctrl-x-t-map)
(define-key my-ctrl-x-t-map (kbd "e") 'my/open-eshell-here)
(define-key my-ctrl-x-t-map (kbd "t") 'my/open-eat-eshell-here)
(define-key my-ctrl-x-t-map (kbd "v") 'my/open-vterm-here)
(define-key my-ctrl-x-t-map (kbd "a") 'my/open-ansi-term-here)

;; C-x g prefix for git (only if git functions are available)
(when (fboundp 'my/magit-status-current-dir)
  (defvar my-ctrl-x-g-map (make-sparse-keymap)
    "Keymap for C-x g prefix.")
  (define-key ctl-x-map (kbd "g") my-ctrl-x-g-map)
  (define-key my-ctrl-x-g-map (kbd "s") 'my/magit-status-current-dir)
  (define-key my-ctrl-x-g-map (kbd "d") 'my/magit-dispatch))

   

   
  
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



(provide 'keybindings)
;;; keybindings.el ends here
