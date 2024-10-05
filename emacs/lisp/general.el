(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer ss/leader-key-def
    ;; :keymaps '(normal insert visual emacs)
    :states '(normal visual insert emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"
    :non-normal-prefix "C-SPC")
  (general-create-definer ss/ctrl-c-keys
    :prefix "C-c"))

;; (general-define-key
;;   :states '(normal visual insert emacs)
;;   :prefix "SPC"
;;   :non-normal-prefix "C-SPC"
;;   "x" 'execute-extended-command :which-key "M-x")

(ss/leader-key-def
  "x" 'execute-extended-command :which-key "M-x")

;;   "bb"  'consult-buffer :which-key "consult-buffer")

(ss/leader-key-def
 "f"  '(:ignore t :which-key "file search")
 "ff" '(project-find-file :which-key "find file")
 "," '(find-file :which-key "find file")
 "fg" '(consult-ripgrep :which-key "live grep")
 "fG" '(consult-git-grep :which-key "live git grep")
 "t"  '(:ignore t :which-key "ui toggles")
 "j"  '(:ignore t :which-key "jump to")
 "jc"  '(:ignore t :which-key "config files")
 ;; "tt" '(my/load-theme :which-key "choose theme")
 "bi"  '(ibuffer :which-key "go to ibuffer")
 "bk"  '(kill-buffer :which-key "kill-buffer")
 "bb"  '(consult-buffer :which-key "consult-buffer")
 ;; "x"   '(extended-execute-command :which-key "M-x")
 "C"  '(compile :which-key "compile")
 "c"  '(recompile :which-key "recompile")
 "R"  '(visual-line-mode :which-key "visual line mode")
 "s"  '(sudo-find-file :which-key "sudo find file")
 "d"  '((lambda () (interactive) (find-file (expand-file-name "."))) :which-key "dired here")
 "js" '((lambda () (interactive) (find-file "~/.local/scripts")) :which-key "scripts")
 "ju" '((lambda () (interactive) (find-file (expand-file-name (concat (getenv "UNI") "/" (getenv "CURRENT_Q"))))) :which-key "uni")
 "ji" '((lambda () (interactive) (find-file (expand-file-name "~/notes/inbox.org"))) :which-key "inbox")
 "jw"  '(:ignore t :which-key "work map")
 "jwi" '((lambda () (interactive) (find-file (expand-file-name "~/notes/work/inbox.org"))) :which-key "work inbox")
 "jwd" '((lambda () (interactive) (find-file (expand-file-name "~/notes/work/daily.org"))) :which-key "work daily")
 "jwc" '((lambda () (interactive) (find-file (expand-file-name "~/notes/work/concepts.org"))) :which-key "work concepts")
 "jcd" '((lambda () (interactive) (find-file (expand-file-name "~/.local/src/dwm/config.def.h"))) :which-key "dwm config")
 "jcn" '((lambda () (interactive) (find-file (expand-file-name (concat my/nixos-directory "flake.nix")))) :which-key "nix config")
 "jcg" '((lambda () (interactive) (find-file (expand-file-name (concat my/guix-directory "config.scm")))) :which-key "guix config")
 "jce" '((lambda () (interactive) (find-file (expand-file-name (concat user-emacs-directory "init.el")))) :which-key "emacs config"))

(global-set-key (kbd "<f5>") #'recompile)


(ss/leader-key-def
  "g"  '(:ignore t :which-key "git commands")
  "gs" '(magit-status :which-key "git status"))
