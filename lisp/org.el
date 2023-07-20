(defun efs/org-mode-setup ()
  (variable-pitch-mode 1)
  (org-indent-mode)
  (visual-line-mode 1))

(use-package org
  ;; :ensure t
  ;; :hook (org-mode . efs/org-mode-setup)
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  ;; ("C-c b" . org-switchb))
  :config
  (setq org-directory "~/notes")
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
  (setq org-descriptive-links t)
  (setq org-capture-templates
        '(("t" "Task" entry  (file+headline "~/notes/inbox.org" "Tasks") "** TODO %?\nContext: %a\n")
          ("i" "Idea" entry  (file+headline "~/notes/inbox.org" "Ideas") "** %?\nContext: %a\n")
          ("e" "Emacs" entry  (file+headline "~/notes/inbox.org" "Emacs") "** TODO %?\nContext: %a\n")
          ("w" "Work" entry  (file+headline "~/notes/work/inbox.org" "Tasks") "* TODO %?\nContext: %a\n")

          ("u" "Uni entries")
          ("uc" "CPD" entry  (file+headline "~/notes/uni/cpd.org" "Tasks") "** %?\n%a\n")
          ("us" "SOA" entry  (file+headline "~/notes/uni/soa.org" "Tasks") "** %?\n%a\n")
          ("ux" "SDX" entry  (file+headline "~/notes/uni/sdx.org" "Tasks") "** %?\n%a\n")
          ("ut" "TXC" entry  (file+headline "~/notes/uni/txc.org" "Tasks") "** %?\n%a\n")
          ("up" "PTI" entry  (file+headline "~/notes/uni/pti.org" "Tasks") "** %?\n%a\n")

          ("p" "Project entries")
          ("pz" "ZeOS" entry  (file+headline "~/notes/uni/soa.org" "ZeOS") "** %?\n%a\n")

          ("w" "Work entries")
          ("wi" "Inbox" entry  (file+headline "~/notes/work/inbox.org" "Captures") "** %?\n%a\n")
          ("wc" "Concept" entry  (file+headline "~/notes/work/concepts.org" "Concepts") "** %?\n%a\n")
          ("wd" "Daily" entry  (file+olp+datetree "~/notes/work/daily.org")
           "* %?\nEntered on %U\n  %i\n  %a")

          ("j" "Journal" entry (file+olp+datetree "~/notes/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a"))))


(defvar prot-org-custom-daily-agenda
  ;; NOTE 2021-12-08: Specifying a match like the following does not
  ;; work.
  ;;
  ;; tags-todo "+PRIORITY=\"A\""
  ;;
  ;; So we match everything and then skip entries with
  ;; `org-agenda-skip-function'.
  `((tags-todo "*"
               ((org-agenda-skip-function '(org-agenda-skip-if nil '(timestamp)))
                (org-agenda-skip-function
                 `(org-agenda-skip-entry-if
                   'notregexp ,(format "\\[#%s\\]" (char-to-string org-priority-highest))))
                (org-agenda-block-separator nil)
                (org-agenda-overriding-header "Important tasks without a date\n")))
    (agenda "" ((org-agenda-span 1)
                (org-deadline-warning-days 0)
                (org-agenda-block-separator nil)
                (org-scheduled-past-days 0)
                ;; We don't need the `org-agenda-date-today'
                ;; highlight because that only has a practical
                ;; utility in multi-day views.
                (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                (org-agenda-format-date "%A %-e %B %Y")
                (org-agenda-overriding-header "\nToday's agenda\n")))
    (agenda "" ((org-agenda-start-on-weekday nil)
                (org-agenda-start-day "+1d")
                (org-agenda-span 3)
                (org-deadline-warning-days 0)
                (org-agenda-block-separator nil)
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                (org-agenda-overriding-header "\nNext three days\n")))
    (agenda "" ((org-agenda-time-grid nil)
                (org-agenda-start-on-weekday nil)
                ;; We don't want to replicate the previous section's
                ;; three days, so we start counting from the day after.
                (org-agenda-start-day "+4d")
                (org-agenda-span 60)
                (org-agenda-show-all-dates nil)
                (org-deadline-warning-days 0)
                (org-agenda-block-separator nil)
                (org-agenda-entry-types '(:deadline))
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                (org-agenda-overriding-header "\nUpcoming deadlines (+60d)\n"))))
  "Custom agenda for use in `org-agenda-custom-commands'.")

(setq org-agenda-custom-commands
      `(("A" "Daily agenda and top priority tasks"
         ,prot-org-custom-daily-agenda)))
;; ("P" "Plain text daily agenda and top priorities"
;;  ,prot-org-custom-daily-agenda
;;  ((org-agenda-with-colors nil
;;   (org-agenda-prefix-format "%t %s")
;;   (org-agenda-current-time-string ,(car (last org-agenda-time-grid)))
;;   (org-agenda-fontify-priorities nil)
;;   (org-agenda-remove-tags t))
;;  ("agenda.txt"))))

;; when org-hide-emphasis-markers is on it shows
;; the markup symbols when the cursor is place inside the word
(use-package org-appear
  ;;:ensure t
  :hook (org-mode . org-appear-mode))

(defun my/search-org-files ()
  (interactive)
  ;; (consult-ripgrep  "~/notes" nil "Search Notes: "))
  (consult-ripgrep  "~/notes"))

(use-package evil-org
  ;;:ensure t
  :after org
  :hook ((org-mode . evil-org-mode)
         (org-agenda-mode . evil-org-mode)
         (evil-org-mode . (lambda () (evil-org-set-key-theme '(navigation todo insert textobjects additional)))))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; (ss/leader-key-def
;;  "o"   '(:ignore t :which-key "org mode")
;;  "ol"  '(:ignore t :which-key "links")
;;  "oli" '(org-insert-link :which-key "insert link")
;;  "ols" '(org-store-link :which-key "store link")
;;  "on"  '(org-toggle-narrow-to-subtree :which-key "toggle narrow")
;;  "os"  '(my/search-org-files :which-key "search notes")
;;  "oI"  '(org-toggle-inline-images :which-key "toggle inline images")
;;  "oS"  '(my/org-screenshot-notes-img :which-key "screenshot")
;;  "oD"  '(my/remove-last-pkm-screenshot :which-key "remove-last-screenshot")
;;  "oa"  '(org-agenda :which-key "status")
;;  ;; "od"  '(org-agenda-day-view :which-key "agenda day view")
;;  ;; "ou"  '(org-todo-list "uni" :which-key "uni tasks")
;;  "ot"  '(org-todo-list :which-key "todos")
;;  "oc"  '(org-capture t :which-key "capture")
;;  "op"  '(org-latex-export-to-pdf t :which-key "export to pdf")
;;  "oxx"  '(org-export-dispatch t :which-key "export")
;;  "oxf"  '(ss/org-export-dispatch-with-folder :which-key "export choosing folder"))

;; (global-set-key (kbd "C-c l") 'org-store-link)
;; (global-set-key (kbd "C-c C-l") 'org-insert-link)

(use-package org-modern
  :disabled t
  ;;:ensure t
  :hook ((org-mode                 . org-modern-mode)
         (org-agenda-finalize-hook . org-modern-agenda))
  :custom ((org-modern-todo t)
           (org-modern-table nil)
           (org-modern-variable-pitch nil)
           (org-modern-block-fringe nil))
  :commands (org-modern-mode org-modern-agenda)
  :init (global-org-modern-mode))


(use-package org-bullets
  ;; :disabled t
  ;;:ensure t
  :hook (org-mode . org-bullets-mode)
  ;; :custom
  ;; (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))
  )

(use-package org-roam
  ;;:ensure t
  ;; :bind (("C-c r l" . org-roam-buffer-toggle)
  ;;        ("C-c r f" . org-roam-node-find)
  ;;        ("C-c r i" . org-roam-node-insert)
  ;;        ("C-c r c" . org-roam-capture)
  ;;        ("C-c r g" . org-roam-graph)
  ;;        ("C-c r u" . org-roam-db-sync)
  ;;        ;; ("C-c r r" . org-roam-ref-find)
  ;;        ;; ("C-c r b" . org-roam-switch-to-buffer)
  ;;        ("C-c r d" . org-roam-dailies-capture-today))
  :custom
  (org-roam-directory org-directory)
  (org-roam-completion-everywhere t)
  (org-roam-completion-system 'default)
  (org-roam-db-autosync-mode)
  (org-roam-capture-templates
   '(("m" "main" plain
      "%?"
      :if-new (file+head "main/${slug}.org"
                         "#+title: ${title}\n#+date: %<%Y-%m-%d>\n")
      ;; :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :immediate-finish t
      :unnarrowed t)
     ("u" "uni" plain
      "%?"
      :if-new (file+head "uni/${slug}.org"
                         "#+title: ${title}\n#+filetags: uni\n\n* Info\n** Material\n** Tasks\n** Horari\n* Teoria")
      :immediate-finish t
      :unnarrowed t)
     ("p" "project" plain "*Project info\n\n** Goals\n\n%?\n\n** Tasks\n\n**\n\n** Dates\n\n"
      :if-new (file+head "projects/${slug}.org" "#+title: ${title}\n#+filetags: project")
      :unnarrowed t)
     ("w" "work" plain ""
      :if-new (file+head "work/${slug}.org" "#+title: ${title}\n#+filetags: project")
      :unnarrowed t)
     ("n" "note" plain
      "** %?\n%a"
      :target (headline "* Captures")
      :immediate-finish t
      :unnarrowed t)
     )))

(setq org-roam-node-display-template
      (concat "${title:*} "
              (propertize "${tags:10}" 'face 'org-tag)))

(ss/leader-key-def
  ;; "or" '(ignore t :which-key "roam")
  "rb" '(org-roam-buffer-toggle :which-key "buffer toggle")
  "rc" '(org-roam-capture :which-key "roam capture")
  "rf" '(org-roam-node-find :which-key "node find")
  "ri" '(org-roam-node-insert :which-key "node insert")
  "rI" '(org-roam-node-insert-immediate :which-key "node insert immediate")
  "ru" '(org-roam-db-sync :which-key "sync roam db"))

(use-package org-roam-ui
  :after org-roam
  ;;:ensure t
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; show folder on display
(cl-defmethod org-roam-node-type ((node org-roam-node))
  "Return the TYPE of NODE."
  (condition-case nil
      (file-name-nondirectory
       (directory-file-name
        (file-name-directory
         (file-relative-name (org-roam-node-file node) org-roam-directory))))
    (error "")))

(setq org-roam-node-display-template
      (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

(custom-set-variables
 ;; '(org-agenda-files (list "~/notes/projects" "~/notes/uni" "~/notes/inbox.org")))
 '(org-agenda-files (list (concat org-directory "/inbox.org"))))
(setq calendar-week-start-day 1)


(defun my/org-screenshot-notes-img ()
  "Take a screenshot into a time stamped unique-named file in the
    img directory under the org-buffer directory and insert a link to this file."
  (interactive)
  (setq img-dir (concat org-directory "/attachments/img"))
  (unless (file-exists-p img-dir)
    (make-directory img-dir))
  (setq filename
        (concat img-dir "/" (format-time-string "%Y%m%d_%H%M%S") ".png"))
  (call-process "import" nil nil nil filename)
  (insert (concat "[[" filename "]]")))


(defun my/org-export-dispatch-with-folder ()
  "Export to a file in a given folder"
  (interactive)
  (let ((out-dir (read-directory-name "Export to directory: ")))
    (unless (file-exists-p out-dir)
      (make-directory out-dir))
    (let ((default-directory out-dir))
      (org-export-dispatch))))



;; (org-babel-do-load-languages
;;    'org-babel-load-languages
;;    '((haskell . t) (emacs-lisp . t) (shell . t) (python . t)
;;      (C . t) (lua . t) (dot . t) (java . t)
;;      (lisp . t) (clojure . t) (scheme . t)
;;      (forth . t) (rust . t)))


(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))

(use-package ob-restclient
  :after org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((restclient . t))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (C . t)
   (shell . t)))


(setq org-confirm-babel-evaluate nil)
(push '("conf-unix" . conf-unix) org-src-lang-modes)

(use-package org-tempo
  :ensure nil
  :after org
  :config
  (let ((templates '(("sh"  . "src sh")
                     ("ash" . "src sh :async")
                     ("el"  . "src emacs-lisp")
                     ("vim" . "src vim")
                     ("cpp" . "src C++ :includes <iostream>  :namespaces std"))))
    (dolist (template templates)
      (push template org-structure-template-alist))))


(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(use-package ob-async)
