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

(defun ssm/org-faces ()
    (set-face-attribute 'org-todo nil :height 0.8)
    (set-face-attribute 'org-level-1 nil :height 1.2)
    (set-face-attribute 'org-level-2 nil :height 1.1))

;; Source - https://stackoverflow.com/a/76642982
;; (custom-set-faces
;;   '(org-level-1 ((t (:inherit outline-1 :height 2.0))))
;;   '(org-level-2 ((t (:inherit outline-2 :height 1.8))))
;;   '(org-level-3 ((t (:inherit outline-3 :height 1.6))))
;;   '(org-level-4 ((t (:inherit outline-4 :height 1.4))))
;;   '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
;;   (set-face-attribute 'org-document-title nil :height 2.0))

(add-hook 'org-mode-hook #'ssm/org-faces)

(use-package olivetti
  :disabled t
  :hook (org-mode . olivetti-mode)
  :config
  ;; Set the width of the text area (can be an integer for characters, or a float for % of window)
  (setq olivetti-body-width 110))

(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode))
  ;; :config
  ;; ;; Customize the look of your headline bullets
  ;; (setq org-superstar-headline-bullets-list '("◉" "○" "✸" "✿" "✤" "✜")))

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
