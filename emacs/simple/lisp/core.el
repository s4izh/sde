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

(save-place-mode 1)
(savehist-mode 1)
;; vertico sorts candidates by history, so a short one makes it forgetful.
(setq history-length 300)

;; Built-in quality of life. Keys that go with these are in keybindings.el.
(recentf-mode 1)           ; recent files, shown by consult-buffer (C-x B)
(repeat-mode 1)            ; C-x o o o, C-x u u u, M-g n n n: repeat the last key
(delete-selection-mode 1)  ; typing or yanking replaces the active region
(electric-pair-mode 1)     ; insert the closing paren/quote
(winner-mode 1)            ; C-c <left> / C-c <right>: undo/redo window layouts

(setq isearch-lazy-count t            ; "3/17" match counter in isearch
      set-mark-command-repeat-pop t   ; C-u C-SPC, then just C-SPC to keep popping
      tab-always-indent 'complete     ; TAB indents, then completes
      help-window-select t            ; focus *Help* so q closes it right away
      dired-dwim-target t             ; copy/rename targets the other dired window
      dired-kill-when-opening-new-dired-buffer t) ; one dired buffer, not a trail

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
