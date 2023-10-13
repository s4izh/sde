(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer ss/leader-key-def
			  :keymaps '(normal insert visual emacs)
			  :prefix "SPC"
			  :global-prefix "C-SPC")
  (general-create-definer ss/ctrl-c-keys
			  :prefix "C-c"))

(ss/leader-key-def
 "ff" '(project-find-file :which-key "find file")
 "," '(find-file :which-key "find file")
 "fg" '(consult-ripgrep :which-key "live grep")
 "fG" '(consult-git-grep :which-key "live git grep")
 "t"  '(:ignore t :which-key "ui toggles")
 "jc"  '(:ignore t :which-key "config files")
 "tt" '(my/load-theme :which-key "choose theme")
 "bi"  '(ibuffer :which-key "go to ibuffer")
 "bb"  '(counsel-switch-buffer :which-key "counsel-switch-buffer")
 "bk"  '(kill-buffer :which-key "kill-buffer")
 "p"  '(previous-buffer :which-key "previous-buffer")
 "n"  '(next-buffer :which-key "next-buffer")
 "C"  '(compile :which-key "compile")
 "c"  '(recompile :which-key "recompile")
 "R"  '(visual-line-mode :which-key "visual line mode")
 "s"  '(sudo-find-file :which-key "sudo find file")
 "d" '(lambda () (interactive) (find-file (expand-file-name ".")))
 "js" '(lambda () (interactive) (find-file "~/.local/scripts"))
 "ju" '(lambda () (interactive) (find-file (expand-file-name (concat (getenv "UNI") "/" (getenv "CURRENT_Q")))))
 "ji" '(lambda () (interactive) (find-file (expand-file-name "~/notes/inbox.org")))
 "jwi" '(lambda () (interactive) (find-file (expand-file-name "~/notes/work/inbox.org")))
 "jwd" '(lambda () (interactive) (find-file (expand-file-name "~/notes/work/daily.org")))
 "jwc" '(lambda () (interactive) (find-file (expand-file-name "~/notes/work/concepts.org")))
 "jcd" '(lambda () (interactive) (find-file (expand-file-name "~/.local/src/dwm/config.def.h")) :which-key "dwm config")
 "jcn" '(lambda () (interactive) (find-file (expand-file-name (concat my/nixos-directory "flake.nix"))) :which-key "nix config")
 "jcg" '(lambda () (interactive) (find-file (expand-file-name (concat my/guix-directory "config.scm"))) :which-key "guix config")
 "jce" '(lambda () (interactive) (find-file (expand-file-name (concat user-emacs-directory "init.el"))) :which-key "emacs config"))

(global-set-key (kbd "<f5>") #'recompile)


(ss/leader-key-def
  "g"  '(:ignore t :which-key "toggles")
  "gs" '(magit-status :which-key "git status"))
