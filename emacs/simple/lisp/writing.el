;;; writing.el --- org-mode tasks/agenda, denote notes, and markdown -*- lexical-binding: t; -*-

(defun ssm/org-toggle-emphasis-markers ()
  "Toggle hiding of org emphasis markers, e.g. the asterisks in *bold*."
  (interactive)
  (setq org-hide-emphasis-markers (not org-hide-emphasis-markers))
  (font-lock-flush)
  (font-lock-ensure)
  (message "org-hide-emphasis-markers: %s" org-hide-emphasis-markers))

(use-package org
  :ensure nil
  :custom
  (org-directory "~/notes/org/")
  (org-agenda-files (list org-directory))
  (org-return-follows-link t)
  (org-hide-leading-stars t)
  (org-hide-emphasis-markers t)
  (org-startup-indented t)
  (org-src-fontify-natively t)
  (org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (org-capture-templates
   '(("t" "Task" entry (file+headline "~/notes/org/tasks.org" "Tasks")
      "* TODO %?\n%U\n")))
  :bind
  (("C-c o c" . org-capture)
   ("C-c o a" . org-agenda)
   ("C-c o l" . org-store-link)
   ("C-c o e" . ssm/org-toggle-emphasis-markers)))

(use-package org-modern
  :disabled t
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)))
  ; :custom
  ; (org-modern-star '("◉" "○" "✸" "✿" "◈")))

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

(use-package denote-org
  :after denote
  :bind
  (("C-c n i" . denote-org-dblock-insert-links)
   ("C-c n I" . denote-org-dblock-insert-missing-links)))

(use-package citar
  :custom
  (citar-bibliography '("~/notes/docs/zotero/zotero.bib"))
  (citar-library-paths '("~/notes/docs/zotero/"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  :hook
  (org-mode . citar-capf-setup)
  :config
  (setq org-cite-global-bibliography citar-bibliography)
  :bind
  (("C-c b i" . citar-insert-citation)
   ("C-c b o" . citar-open)))

(use-package citar-denote
  :after (citar denote)
  :config
  (citar-denote-mode)
  :bind
  (("C-c n c" . citar-denote-dwim)
   ("C-c n o" . citar-denote-open-note)
   ("C-c n k" . citar-denote-add-citekey)))

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode))
