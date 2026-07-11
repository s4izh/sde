;;; core.el --- persistence and baseline behavior -*- lexical-binding: t; -*-

(setq make-backup-files nil)         ; don't clutter directories with ~ backup files
(setq auto-save-default nil)         ; don't create annoying #autosave# files

;; no gui dialogs
(setq use-dialog-box nil)
(setq use-file-dialog nil)
(setq use-short-answers t) ; use y/n not yes/no

;; autorefresh buffers
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

(setq backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      make-backup-files t    ; keep making backups for safety...
      vc-make-backup-files t ; even for files tracked by Git
      version-control t      ; use version numbers for backups
      kept-old-versions 2
      kept-new-versions 5
      delete-old-versions t)

(setq create-lockfiles nil)

(save-place-mode 1)
(savehist-mode 1)
(setq history-length 25)


(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode))

;; Start the Emacs server so `emacsclient' can reuse this session. Guarded so a
;; second instance doesn't error trying to start an already-running server.
(use-package server
  :ensure nil
  :config
  (unless (server-running-p)
    (server-start)))
