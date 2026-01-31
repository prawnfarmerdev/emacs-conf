;;; core.el --- Core Emacs configuration (package management, UI, editor behavior, fonts) -*- lexical-binding: t -*-

;;; Commentary:
;; Core configuration including package management, UI settings, editor behavior,
;; font configuration, and platform detection.



;;==============================================================================
;; STARTUP OPTIMIZATION
;;==============================================================================

;; Increase GC threshold during startup to reduce garbage collection pauses
(defvar my/default-gc-cons-threshold gc-cons-threshold)
(defvar my/default-gc-cons-percentage gc-cons-percentage)
(setq gc-cons-threshold (* 128 1024 1024))  ; 128MB
(setq gc-cons-percentage 0.6)

;; Reset GC after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold my/default-gc-cons-threshold
                  gc-cons-percentage my/default-gc-cons-percentage)
            (message "GC thresholds reset to defaults")))

;;==============================================================================
;; PACKAGE MANAGEMENT
;;==============================================================================

;; Disable automatic package refresh to avoid network errors
(setq package-check-signature nil)
(setq package-quickstart t)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                          ("gnu" . "https://elpa.gnu.org/packages/")
                          ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; Initialize package system (called automatically by Emacs 27+, but call early for use-package)
(package-initialize)
;; Ensure org-roam is included in packages to install
(add-to-list 'package-selected-packages 'org-roam)

;; Auto-install missing packages from package-selected-packages
(defun my/ensure-packages-installed ()
  "Ensure all packages in `package-selected-packages` are installed."
  (when (boundp 'package-selected-packages)
    ;; Refresh package contents if needed
    (unless package-archive-contents
      (package-refresh-contents))
    
    ;; Install each missing package
    (dolist (pkg package-selected-packages)
      (unless (package-installed-p pkg)
        (message "Installing missing package: %s" pkg)
        (condition-case err
            (package-install pkg)
          (error
           (message "Failed to install package %s: %s" pkg (error-message-string err))))))))

;; Run package installation (can be commented out after first run)
(my/ensure-packages-installed)

;;==============================================================================
;; SYSTEM PATH
;;==============================================================================

;; Ensure system directories are in exec-path for language servers
(dolist (dir '("/usr/sbin" "/usr/local/sbin" "/sbin" "/bin"))
  (when (and (file-directory-p dir)
             (not (member dir exec-path)))
    (add-to-list 'exec-path dir)))

;;==============================================================================
;; NATIVE COMPILATION (JIT)
;;==============================================================================

;; Enable native compilation for packages (Emacs 28+)
(when (fboundp 'native-comp-available-p)
  ;; Ensure packages are natively compiled when installed
  (setq package-native-compile t)
  ;; Enable deferred background compilation
  (setq native-comp-deferred-compilation t)
  
  ;; Helper function to compile all installed packages
  (defun my/native-compile-all-packages ()
    "Compile all installed packages natively in the background.
Useful after updating Emacs or when native compilation was disabled."
    (interactive)
    (when (fboundp 'native-compile-async)
      (dolist (dir (list (expand-file-name "elpa" user-emacs-directory)
                         (expand-file-name "straight" user-emacs-directory)
                         (expand-file-name "~/.config/emacs/elpa")))
        (when (file-directory-p dir)
          (native-compile-async dir t)))
      (message "Native compilation started for all packages in background")))
  
  ;; Helper function to compile configuration files
  (defun my/native-compile-config ()
    "Compile configuration files natively for faster loading."
    (interactive)
    (when (fboundp 'native-compile-async)
      (native-compile-async (expand-file-name "config" user-emacs-directory) t)
      (message "Native compilation started for configuration files"))))

;;==============================================================================
;; WHICH-KEY
;;==============================================================================

(use-package which-key
  :ensure t
  :defer t
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
;; VTERM CONFIGURATION
;;==============================================================================

(use-package vterm
  :ensure t
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000))

;;==============================================================================
;; FONT CONFIGURATION (Wayland / HiDPI / NO XLFD)
;;==============================================================================

(setq-default
 x-gtk-use-system-tooltips nil
 inhibit-compacting-font-caches t)

(setq font-use-system-font nil)

(defvar my/default-font-size 130)

;; Default font selection
(defvar my/default-font
  (font-spec :family "CaskaydiaCove Nerd Font"
             :size 18))

(defun my/apply-fonts (&optional frame)
  "Apply fonts to FRAME (or selected frame)."
  (with-selected-frame (or frame (selected-frame))
    (set-frame-font my/default-font t t)
    (set-face-attribute 'default nil
                        :family "CaskaydiaCove Nerd Font"
                        :height my/default-font-size)
    (set-face-attribute 'fixed-pitch nil
                        :family "CaskaydiaCove Nerd Font"
                        :height my/default-font-size)
    (set-face-attribute 'variable-pitch nil
                        :family "CaskaydiaCove Nerd Font"
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
(setq ring-bell-function 'ignore)


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

;;==============================================================================
;; DIRED CONFIGURATION
;;==============================================================================

;; Sort directories first (requires GNU ls)
(setq dired-listing-switches "-alh --group-directories-first")

;; Enable dired-dwim-target (copy/move between two dired buffers)
(setq dired-dwim-target t)

;; Auto-revert dired buffers when files change
(add-hook 'dired-mode-hook 'auto-revert-mode)

(provide 'core)
;;; core.el ends here
