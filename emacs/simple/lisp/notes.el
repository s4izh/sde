;;; notes.el --- org-mode tasks/agenda and denote notes -*- lexical-binding: t; -*-

(use-package org
  :ensure nil
  :custom
  (org-directory "~/notes/org/")
  (org-agenda-files (list org-directory))
  (org-return-follows-link t)
  (org-hide-leading-stars t)
  (org-src-fontify-natively t)
  (org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (org-capture-templates
   '(("t" "Task" entry (file+headline "~/notes/org/tasks.org" "Tasks")
      "* TODO %?\n%U\n")))
  :bind
  (("C-c o c" . org-capture)
   ("C-c o a" . org-agenda)
   ("C-c o l" . org-store-link)))

(use-package denote
  :custom
  (denote-directory "~/notes/org/")
  :bind
  (("C-c n n" . denote)
   ("C-c n f" . denote-open-or-create)
   ("C-c n l" . denote-link-or-create)
   ("C-c n b" . denote-find-backlink)
   ("C-c n B" . denote-link-backlinks)
   ("C-c n r" . denote-rename-file)))
