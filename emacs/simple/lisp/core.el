;;; core.el --- persistence and baseline behavior -*- lexical-binding: t; -*-

(setq auto-save-default nil)         ; don't create annoying #autosave# files

;; no gui dialogs
(setq use-dialog-box nil)
(setq use-file-dialog nil)
(setq use-short-answers t) ; use y/n not yes/no

;; autorefresh buffers
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

;; Backups go to $XDG_STATE_HOME, not ~/.emacs.d: that is a symlink into the
;; sde repo, and backups include notes and other private files.
(setq backup-directory-alist
      `(("." . ,(expand-file-name "emacs/backups/"
                                  (or (getenv "XDG_STATE_HOME") "~/.local/state"))))
      make-backup-files t    ; keep making backups for safety...
      vc-make-backup-files t ; even for files tracked by Git
      version-control t      ; use version numbers for backups
      kept-old-versions 2
      kept-new-versions 5
      delete-old-versions t)

(setq create-lockfiles nil)

;; Undo as a tree rather than a linear ring: C-x u opens the visualizer, where
;; you can move between branches created by undoing and then editing again.
;; Not evil-specific -- evil-setup.el just points `evil-undo-system' at it.
(use-package undo-tree
  :disabled t
  :config
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t
        ;; Keep the .~undo-tree~ history files out of the source directories.
        undo-tree-history-directory-alist
        `(("." . ,(expand-file-name "undo-tree" user-emacs-directory))))
  (global-undo-tree-mode 1))

(save-place-mode 1)
(savehist-mode 1)
(setq history-length 25)

(setq tramp-use-connection-sharing t)

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
