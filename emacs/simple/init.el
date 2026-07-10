;;; init.el --- bootstrap: package.el and module loading -*- lexical-binding: t; -*-

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Set to t on machines where packages are provided by Guix/Nix
;; instead of package.el. No autodetection yet, flip by hand per machine.
(defvar ssm/managed-packages nil)

(setq use-package-always-ensure (not ssm/managed-packages))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(load "core")
(load "appearance")
(load "completion")
(load "programming")
(load "tools")
(load "notes")
(load "ai")
(load "keybindings")
