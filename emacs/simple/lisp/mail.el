;;; mail.el --- notmuch.el over the shared notmuch database -*- lexical-binding: t; -*-

;; Mail is synced, indexed and tagged outside Emacs (mbsync + notmuch, see
;; nixos/home/sergio/work/bsc-mail.nix); aerc reads the same database. Here
;; Emacs only reads, tags and composes. notmuch.el comes from nixpkgs so it
;; always matches the notmuch binary, hence :ensure nil.

(use-package notmuch
  :ensure nil
  :commands (notmuch notmuch-search notmuch-mua-new-mail)
  :bind (("C-c m m" . notmuch)                ; SPC m m: hello screen
         ("C-c m s" . notmuch-search)         ; SPC m s: search
         ("C-c m c" . notmuch-mua-new-mail))  ; SPC m c: compose
  :custom
  ;; Same virtual folders as aerc (~/.config/aerc/bsc-queries).
  (notmuch-saved-searches
   '((:name "inbox"  :query "tag:inbox"                  :key "i")
     (:name "unread" :query "tag:unread and not tag:lista" :key "u")
     (:name "todo"   :query "tag:todo"                   :key "t")
     (:name "listas" :query "tag:lista and tag:unread"   :key "l")
     (:name "noti"   :query "tag:noti and tag:unread"    :key "n")
     (:name "sent"   :query "tag:sent"                   :key "s")))
  (notmuch-search-oldest-first nil)
  (notmuch-show-all-tags-list t)
  ;; Sent mail goes to the server's Sent folder (mbsync uploads it) and is
  ;; tagged like the post-new rules would tag it.
  (notmuch-fcc-dirs "bsc/Sent +sent -unread -inbox")
  ;; Archiving = leave the inbox (and the unread list).
  (notmuch-archive-tags '("-inbox" "-unread"))
  ;; Remote images stay blocked (tracking pixels); inline ones still show.
  (notmuch-show-text/html-blocked-images ".")
  ;; Send through msmtp, which picks the account from the From: header.
  (send-mail-function #'message-send-mail-with-sendmail)
  (message-send-mail-function #'message-send-mail-with-sendmail)
  (sendmail-program "msmtp")
  (message-sendmail-envelope-from 'header)
  (mail-specify-envelope-from t)
  (mail-envelope-from 'header)
  (message-kill-buffer-on-exit t))

;;; mail.el ends here
