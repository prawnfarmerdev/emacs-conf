;;; init.el --- Minimal Emacs config for terminal testing -*- lexical-binding: t -*-

;;; Commentary:
;; Minimal configuration for testing terminal options on Windows
;; Focus: SSH with MFA support in various terminal emulators

;; Startup timing
(defvar my/start-time (current-time))
(defun my/display-startup-time ()
  (let ((elapsed (float-time (time-subtract (current-time) my/start-time))))
    (message "Emacs loaded in %.2f seconds" elapsed)))

;; Add config directory to load path
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

;; Platform detection
(defvar my/is-windows (eq system-type 'windows-nt)
  "Non-nil if running on Windows.")

;; Package management - keep it minimal
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Load modules
(load "core")          ; Basic settings
(load "helpers")       ; Terminal helper functions  
(load "keybindings")   ; Keybindings
(when (file-exists-p (expand-file-name "config/ssh.el" user-emacs-directory))
  (load "ssh"))        ; SSH sessionizer if exists

;; Display startup time
(add-hook 'emacs-startup-hook #'my/display-startup-time)

(provide 'init)