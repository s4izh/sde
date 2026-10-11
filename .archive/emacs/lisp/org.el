(use-package org
  ;; :ensure t
  ;; :hook (org-mode . efs/org-mode-setup)
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  ;; ("C-c b" . org-switchb))
  :config
  (setq org-src-window-setup 'current-window)
  (setq org-ellipsis " ▾")
  (setq org-hide-emphasis-markers t)
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-src-tab-acts-natively t)
  (setq org-log-into-drawer t)
  (setq org-startup-indented t)           ;; Indent according to section
  (setq org-startup-with-inline-images t) ;; Display images in-buffer by default
  (setq org-startup-folded t)
  ;; test
  (setq org-return-follows-link t)
  (setq org-mouse-1-follows-link t)

  ;; Display links as the description provided
  (setq org-descriptive-links t))

(use-package org-bullets
  :ensure t
  :config
  (org-bullets-mode))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)))  ; Enable shell scripting

(setq org-confirm-babel-evaluate nil)
