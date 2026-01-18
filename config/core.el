;;; core.el --- Core Emacs configuration (package management, UI, editor behavior, fonts)

;;; Commentary:
;; Core configuration including package management, UI settings, editor behavior,
;; font configuration, and platform detection.

;;==============================================================================
;; PLATFORM DETECTION
;;==============================================================================

(defvar my/is-windows (memq system-type '(windows-nt ms-dos cygwin))
  "Non-nil if running on Windows.")

(defvar my/is-linux (eq system-type 'gnu/linux)
  "Non-nil if running on Linux.")

(defvar my/is-mac (eq system-type 'darwin)
  "Non-nil if running on macOS.")

;;==============================================================================
;; STARTUP OPTIMIZATION
;;==============================================================================

;; Increase GC threshold during startup to reduce garbage collection pauses
;; (defvar my/default-gc-cons-threshold gc-cons-threshold)
;; (defvar my/default-gc-cons-percentage gc-cons-percentage)
;; (setq gc-cons-threshold (* 128 1024 1024))  ; 128MB
;; (setq gc-cons-percentage 0.6)

;; Reset GC after startup
;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (setq gc-cons-threshold my/default-gc-cons-threshold
;;                   gc-cons-percentage my/default-gc-cons-percentage)
;;             (message "GC thresholds reset to defaults")))

;;==============================================================================
;; PACKAGE MANAGEMENT
;;==============================================================================

;; Disable automatic package refresh to avoid network errors
;; (setq package-check-signature nil)
;; (setq package-quickstart t)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                          ("melpa-mirror" . "https://www.mirrorservice.org/sites/melpa.org/packages/")
                          ("gnu" . "https://elpa.gnu.org/packages/")
                          ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; Initialize package system (called automatically by Emacs 27+, but call early for use-package)
(package-initialize)

;;==============================================================================
;; WHICH-KEY
;;==============================================================================

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.0
        which-key-idle-secondary-delay 0.0))

;;==============================================================================
;; GARBAGE COLLECTOR OPTIMIZATION
;;==============================================================================

(use-package gcmh
  :ensure t
  :config
  (gcmh-mode 1)
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold (* 32 1024 1024)))

;;==============================================================================
;; FONT CONFIGURATION (Wayland / HiDPI / NO XLFD)
;;==============================================================================

(setq-default
 x-gtk-use-system-tooltips nil
 inhibit-compacting-font-caches t)

(setq font-use-system-font nil)

(defvar my/default-font-size 130)

;; Default font selection based on platform
(defvar my/default-font
  (if my/is-windows
      (font-spec :family "Cascadia Code"
                 :weight 'normal
                 :size 12)
    (font-spec :family "CaskaydiaCove Nerd Font"
               :weight 'semi-bold
               :size 12)))

(defun my/apply-fonts (&optional frame)
  "Apply fonts to FRAME (or selected frame)."
  (with-selected-frame (or frame (selected-frame))
    (set-frame-font my/default-font t t)
    (set-face-attribute 'default nil
                        :family (if my/is-windows "Cascadia Code" "CaskaydiaCove Nerd Font")
                        :weight (if my/is-windows 'normal 'semi-bold)
                        :height my/default-font-size)
    (set-face-attribute 'fixed-pitch nil
                        :family (if my/is-windows "Cascadia Code" "CaskaydiaCove Nerd Font")
                        :weight (if my/is-windows 'normal 'semi-bold)
                        :height my/default-font-size)
    (set-face-attribute 'variable-pitch nil
                        :family (if my/is-windows "Cascadia Code" "CaskaydiaCove Nerd Font")
                        :height my/default-font-size)))

(my/apply-fonts)
(add-hook 'after-make-frame-functions #'my/apply-fonts)

(pixel-scroll-precision-mode 1)

;;==============================================================================
;; UI SETTINGS
;;==============================================================================

(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(setq display-line-numbers-width 5)
(setq display-line-numbers-grow-only nil)

;; Disable line numbers in terminal buffers
(add-hook 'eshell-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'term-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'shell-mode-hook (lambda () (display-line-numbers-mode -1)))
;; Disable line highlighting in terminal buffers
(add-hook 'eshell-mode-hook (lambda () (hl-line-mode -1)))
(add-hook 'term-mode-hook (lambda () (hl-line-mode -1)))
(add-hook 'vterm-mode-hook (lambda () (hl-line-mode -1)))
(add-hook 'shell-mode-hook (lambda () (hl-line-mode -1)))
;; Set black background for terminal input
(defun my/terminal-set-black-background ()
  "Set black background for terminal buffers."
  (message "Setting black background for %s" (buffer-name))
  ;; Core faces
  (face-remap-add-relative 'default :background "#000000")
  (face-remap-add-relative 'hl-line :background "#000000")
  (face-remap-add-relative 'region :background "#073642")
  ;; Eshell-specific faces if available
  (when (fboundp 'eshell-mode)
    (dolist (face '(eshell-prompt-face
                    eshell-ls-directory-face
                    eshell-ls-executable-face
                    eshell-ls-missing-face
                    eshell-ls-product-face
                    eshell-ls-readonly-face
                    eshell-ls-special-face
                    eshell-ls-symlink-face
                    eshell-ls-unreadable-face
                    eshell-ls-archive-face
                    eshell-ls-backup-face
                    eshell-ls-clutter-face
                    eshell-ls-date-face
                    eshell-ls-missing-face
                    eshell-ls-permissions-face
                    eshell-ls-product-face
                    eshell-ls-size-face
                    eshell-ls-todo-face))
      (when (facep face)
        (face-remap-add-relative face :background "#000000")))))
(add-hook 'eshell-mode-hook #'my/terminal-set-black-background)
(add-hook 'term-mode-hook #'my/terminal-set-black-background)
(add-hook 'vterm-mode-hook #'my/terminal-set-black-background)
(add-hook 'shell-mode-hook #'my/terminal-set-black-background)

(setq idle-update-delay 0.0
      cursor-in-non-selected-windows nil
      highlight-nonselected-windows nil)

;;==============================================================================
;; EDITOR BEHAVIOR
;;==============================================================================

(setq-default indent-tabs-mode nil
              tab-width 4
              truncate-lines t)

(setq truncate-partial-width-windows nil)

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(save-place-mode 1)
(recentf-mode 1)
(setq recentf-max-saved-items 100)

(show-paren-mode 1)
(global-hl-line-mode 1)

(setq show-paren-delay 0
      scroll-margin 8
      scroll-conservatively 101
      scroll-preserve-screen-position t
      fast-but-imprecise-scrolling t
      redisplay-skip-fontification-on-input t)

(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist default-file-name-handler-alist)))

(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      auto-save-default t
      auto-save-timeout 20
      auto-save-interval 200)

(provide 'core)
;;; core.el ends here