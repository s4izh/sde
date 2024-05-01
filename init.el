;; -*- lexical-binding: t; -*-

;; This file bootstraps the configuration, which is divided into
;; a number of other files inside the `lisp' directory.

(setq user-emacs-directory "~/.emacs.d/")

(setq package-archives
  '(("melpa" . "https://melpa.org/packages/")
    ("org" . "https://orgmode.org/elpa/")
    ("elpa" . "https://elpa.gnu.org/packages/")))

;; (package-initialize)

;; (setq use-package-always-ensure t)

;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))
;; (eval-when-compile (require 'use-package))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Always use straight to install on systems other than Linux
(setq straight-use-package-by-default (not (eq system-type 'gnu/linux)))

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

;; Load the helper package for commands like `straight-x-clean-unused-repos'
(require 'straight-x)

;;; my own stuff
(use-package custom-variables
  :ensure nil
  :no-require t
  :demand t
  :init
  (defvar my/is-termux
    (string-suffix-p
     "Android" (string-trim (shell-command-to-string "uname -a")))
    "Value indicating if Emacs is currently running in termux.")
  (defvar my/is-terminal
    (not window-system)
    "Value indicating if Emacs is currently running in a terminal.")
  (defvar my/my-system
    (if (string-equal user-login-name "sergio")
        t
      nil)
    "Non-nil value if this is my system.")
  (defvar my/is-linux
    (if (eq system-type 'gnu-linux)
        t
      nil)
    "Value indicating if running on linux")
  (defvar my/is-arch
    (if (string-search
         "Arch Linux" (string-trim (shell-command-to-string "cat /etc/issue")))
        t
      nil)
    "Value indicating if Emacs is currently running on Arch Linux.")
  (defvar my/is-guix
    (if (string-search 
         "GNU system" (string-trim (shell-command-to-string "cat /etc/issue")))
        t
      nil)
    "Value indicating if Emacs is currently running on Guix.")
  (defvar my/is-nixos
    (if (string-search
         "NixOS" (string-trim (shell-command-to-string "cat /etc/issue")))
        t
      nil)
    "Value indicating if Emacs is currently running on NixOS.")
  (defvar my/is-ubuntu
    (if (string-search
         "Ubuntu" (string-trim (shell-command-to-string "cat /etc/issue")))
        t
      nil)
    "Value indicating if Emacs is currently running on Ubuntu.")
  (defvar my/guix-directory
    "~/.config/guix/"
    "Path to my GNU Guix configuration.")
  (defvar my/nixos-directory
    "~/.local/src/nixos/"
    "Path to my NixOS configuration.")
  (defvar my/work
    (if (string-search "work" (shell-command-to-string "cat /etc/hostname"))
        t
      nil)
    "Non-nil value if this is my work system."))


(unless my/is-guix
  (setq use-package-always-ensure t))

(setenv "EDITOR" "emacsclient")
(setenv "PAGER" "cat")
(setenv "READER" "emacsclient")
(setenv "EMACS" "yes")

(load (concat user-emacs-directory
              "lisp/system.el"))

(load (concat user-emacs-directory
              "lisp/appearance.el"))

(use-package undo-fu
  :bind (("C-x u"   . undo-fu-only-undo)
         ("C-/"     . undo-fu-only-undo)
         ("C-z"     . undo-fu-only-undo)
         ("C-S-z"   . undo-fu-only-redo)
         ("C-x C-u" . undo-fu-only-redo)
         ("C-?"     . undo-fu-only-redo)))

(use-package undo-fu-session ; Persistant undo history
  :demand t
  :config (global-undo-fu-session-mode))

(use-package undo-tree
  :disabled t
  :config
  (setq undo-tree-auto-save-history nil)
  (global-undo-tree-mode 1))

(defalias 'yes-or-no-p 'y-or-n-p)

(load (concat user-emacs-directory
              "lisp/evil.el"))

(use-package async
  :defer t
  :init
  (dired-async-mode 1))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; auto revert buffers on changes
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; no littering
;; (setq user-emacs-directory "~/.cache/emacs")
(use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(setq custom-file (locate-user-emacs-file "custom-vars.el"))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

(setq auto-save-file-name-transforms
  `((".*" ,temporary-file-directory t)))
(load custom-file 'noerror 'nomessage)

;; (load (concat user-emacs-directory
;;               "lisp/modeline.el"))

(use-package dired
  :ensure nil
  :commands (dired)
  :hook ((dired-mode . hl-line-mode)
         (dired-mode . dired-omit-mode)
         (dired-mode . dired-hide-details-mode))
  :bind (:map dired-mode-map
              ("-" . dired-up-directory))
  :init

  (use-package dired-launch
    :ensure t
    :config
    (setq dired-launch-default-launcher '("xdg-open")))

  (setq dired-bind-jump nil)
  :config
  ;; (setq dired-listing-switches "-aghoA --group-directories-first")
  ;; (setq dired-listing-switches "--group-directories-first")
  ;;;;; Hide . and .. in dired
  ;; (setq dired-omit-files
  ;;       (setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..*$"))

  ;; When there are two Dired buffers side-by-side make Emacs
  ;; automatically suggest the other one as the target of copy or rename
  ;; operations.  Remember that you can always use M-p and M-n in the
  ;; minibuffer to cycle through the history, regardless of what this
  ;; does.  (The "dwim" stands for "Do What I Mean".)
  (setq dired-dwim-target t)

  ;; Teach Dired to use a specific external program with either the
  ;; `dired-do-shell-command' or `dired-do-async-shell-command' command
  ;; (with the default keys, those are bound to `!' `&', respectively).
  ;; The first string is a pattern match against file names.  The
  ;; remaining strings are external programs that Dired will provide as
  ;; suggestions.  Of course, you can always type an arbitrary program
  ;; despite these defaults.
  (setq dired-guess-shell-alist-user
        '(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh" "xdg-open")
          ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open")
          (".*" "xdg-open")))

  ;;;;; xdg-open integration
  (require 'dired-x)
  ;; prevent opening extra dired buffers
  ;; emacs 28
  (setq dired-kill-when-opening-new-dired-buffer t))

;;; COMPLETION
(use-package vertico
  :init
  ;; Enable vertico using the vertico-flat-mode
  (require 'vertico-directory)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

  (use-package orderless
    :commands (orderless)
    :custom (completion-styles '(orderless flex)))
  ;; (load (concat user-emacs-directory
  ;;               "lisp/affe-config.el"))
  ;; help messages
  (use-package marginalia
    :custom
    (marginalia-annotators
     '(marginalia-annotators-heavy marginalia-annotators-light nil))
    :init
    (marginalia-mode))
  (vertico-mode t)
  :config
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;;; MAGIT
(use-package magit
  :defer t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; (use-package forge
;;   :ensure t
;;   :after magit)

;; (use-package magit-delta
;;   :disabled t
;;   :ensure t
;;   :hook (magit-mode . magit-delta-mode))

;;; parenthesis
(use-package smartparens
  :hook (prog-mode . smartparens-mode))

;; (use-package rainbow-delimiters
;;   :init
;;   :hook (prog-mode . rainbow-delimiters-mode))

;; remove trailing whitespaces
(use-package ws-butler
  :ensure t
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

(load (concat user-emacs-directory
              "lisp/general.el"))

(load (concat user-emacs-directory
              "lisp/org.el"))

; (unless my/is-ubuntu
;   (load (concat user-emacs-directory
;                 "lisp/ai2.el")))

(setq tramp-default-method "ssh")

(load (concat user-emacs-directory
               "lisp/dev.el"))

;;;; Code Completion
(use-package corfu
  ;; Optional customizations
  :hook((emacs-lisp-mode . corfu-mode))
  :custom
  (corfu-cycle t)                 ; Allows cycling through candidates
  (corfu-auto t)                  ; Enable auto completion
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-preview-current 'insert) ; Do not preview current candidate
  (corfu-preselect-first nil)
  (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets

  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
              ("M-SPC"      . corfu-insert-separator)
              ("TAB"        . corfu-next)
              ([tab]        . corfu-next)
              ("S-TAB"      . corfu-previous)
              ([backtab]    . corfu-previous)
              ;; ("<return>" . corfu-complete)
              ("C-y" . corfu-complete) ;; doesnt work idk why
              ("RET"        . nil))

  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode) ; Popup completion info
  :config
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local corfu-quit-at-boundary t
                              corfu-quit-no-match t
                              corfu-auto nil)
              (corfu-mode))))

(use-package corfu-terminal
  :straight (:host codeberg :repo "akib/emacs-corfu-terminal" :branch "master"))

(unless (display-graphic-p)
  (corfu-terminal-mode +1))

(use-package consult
  :after vertico
  :bind (("C-x b"       . consult-buffer)
         ;; ("C-x C-k C-k" . consult-kmacro)
         ;; ("M-y"         . consult-yank-pop)
         ;; ("M-g g"       . consult-goto-line)
         ;; ("M-g M-g"     . consult-goto-line)
         ;; ("M-g f"       . consult-flymake)
         ;; ("M-g i"       . consult-imenu)
         ;; ("M-s l"       . consult-line)
         ;; ("M-s L"       . consult-line-multi)
         ;; ("M-s u"       . consult-focus-lines)
         ;; ("M-s g"       . consult-ripgrep)
         ;; ("M-s M-g"     . consult-ripgrep)
         ;; ("C-x C-SPC"   . consult-global-mark)
         ;; ("C-c n"       . consult-org-agenda)
         ("C-c m"       . my/notegrep))
         ;; :map help-map
         ;; ("a" . consult-apropos)
         ;; :map minibuffer-local-map
         ;; ("M-r" . consult-history))
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  :config
  (defun my/notegrep ()
    "Use interactive grepping to search my notes"
    (interactive)
    (consult-ripgrep org-directory))
  (recentf-mode t))

(use-package consult-dir
  :ensure t
  :bind (("C-x C-j" . consult-dir)
         ;; :map minibuffer-local-completion-map
         :map vertico-map
         ("C-x C-j" . consult-dir)))

(use-package embark
  :bind(("C-," . embark-act)))

;;; PASS
(use-package password-store
  :commands (password-store-copy
             password-store-insert
             password-store-generate))

;; Authenticte with auth-source-pass
;; (use-package auth-source-pass
;;   :after password-store
;;   :config
;;   (auth-source-pass-enable))

;; (load (concat user-emacs-directory
;;               "lisp/guix.el"))

(use-package pdf-tools
  :config
  (pdf-tools-install))

(defun sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

;; (setq projectile-project-search-path '("~/personal/" "~/uni/3q2" ("~/github" . 1)))

(use-package popper
  ;;:ensure t ; or :straight t
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"))
          ;; help-mode))
          ;; compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints

(use-package vterm
  :bind (
          ;;("C-x t" . vterm)
         :map vterm-mode-map
         ("M-p" . vterm-send-up)
         ("M-n" . vterm-send-down))

  :commands vterm
  :custom (vterm-max-scrollback 10000)
  :init (when my/my-system
          (setq term-prompt-regexp ".*ᛋ")))

(use-package vterm-toggle)

(ss/leader-key-def
  "k" '(vterm-toggle :which-key "toggle vterm"))

(use-package eshell
  :bind ("C-x E" . eshell))

(use-package em-alias
  :ensure nil
  :after eshell
  :config
  (add-hook 'eshell-mode-hook
            (lambda ()
              (eshell/alias "e" "find-file $1")
              (eshell/alias "ee" "find-file-other-window $1")
              (eshell/alias "v" "view-file $1")
              (eshell/alias "o" "crux-open-with $1"))))

;; (load (concat user-emacs-directory
;;               "lisp/exwm-config.el"))

(defun my/compiler-select (file)
  (interactive "f")
  (start-process-shell-command "test" "*test*" (format "compiler %s" file)))

(defun my/compiler ()
  (interactive)
  (start-process-shell-command "my/compiler" "*my/compiler*" (format "compiler %s" buffer-file-name)))

(defun my/compiler-hook ()
  (interactive)
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook 'my/compiler))

;; (use-package stumpwm-mode)

(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "ctags -f TAGS -e -R %s" (directory-file-name dir-name))))

(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "find . -name '*.c' -o -name '*.h' -o -name '*.cpp' -o -name '*.hpp' | xargs etags" (directory-file-name dir-name))))

(defun my/kill-current-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") 'my/kill-current-buffer)
(keymap-global-set "C-x k" 'my/kill-current-buffer)

;; (use-package treesit-auto
;;   :ensure t
;;   :config
;;   (global-treesit-auto-mode))

(use-package compile
  :ensure nil
  :config
  (setq compilation-scroll-output t))

(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

(setq explicit-shell-file-name "/bin/bash")
(setq vterm-shell "/bin/bash")

(defun spawn-shell (name)
  (interactive "MName of shell buffer to create: ")
  (pop-to-buffer (get-buffer-create (generate-new-buffer-name (concat "shell-" name))))
  (shell (current-buffer)))

(defun spawn-vterm (name)
  (interactive "MName of shell buffer to create: ")
    (vterm (concat "vterm-" name))
    (switch-to-buffer (concat "vterm-" name)))

(defun switch-to-shell-or-vterm-buffer ()
  "Switch to a shell or vterm buffer from a menu."
  (interactive)
  (let ((matching-buffers (cl-remove-if-not
                          (lambda (buffer)
                            (or (string-match-p "shell" (buffer-name buffer))
                                (string-match-p "vterm" (buffer-name buffer))))
                          (buffer-list))))
    (if matching-buffers
        (let ((buffer-names (mapcar 'buffer-name matching-buffers)))
          (let ((chosen-buffer (completing-read "Switch to buffer: " buffer-names)))
            (switch-to-buffer chosen-buffer)))
      (message "No shell or vterm buffers found."))))

(global-set-key (kbd "C-c s s") 'switch-to-shell-or-vterm-buffer)
(global-set-key (kbd "C-c s n") 'spawn-shell)
(global-set-key (kbd "C-c s v") 'spawn-vterm)

(setq previous-buffer nil)

(defun toggle-vterm-buffer ()
  "Toggle a vterm buffer with the previous buffer."
  (interactive)
  (if (equal (buffer-name (current-buffer)) "*vterm*")
      (if previous-buffer
          (switch-to-buffer previous-buffer))
    (setq previous-buffer (current-buffer))
    (unless (get-buffer "*vterm*")
      (vterm "*vterm*"))
    (switch-to-buffer "*vterm*")))

(global-set-key (kbd "C-x t") 'toggle-vterm-buffer)

(ss/leader-key-def
  "t" '(toggle-vterm-buffer :which-key "toggle vterm"))

(use-package autorevert
  :ensure nil
  :defer 1
  :init (global-auto-revert-mode t))

(use-package savehist
  :defer 2
  :init (savehist-mode t)) ; Save command history

(use-package eldoc
  :defer 10
  :init
  (setq eldoc-echo-area-display-truncation-message t)
  (setq eldoc-echo-area-use-multiline-p nil)
  (global-eldoc-mode t))

(use-package lab
  :defer 10
  :config
  (setq lab-host (getenv "GITLAB_HOST"))
  (setq lab-token (getenv "GITLAB_TOKEN")))


(defun ssh-to-config-host ()
  "Open an SSH connection to a host from the SSH config file."
  (interactive)
  (let* ((config-file "~/.ssh/config")
         (hosts (ssh-config-list-hosts config-file))
         (chosen-host (completing-read "Choose an SSH config host: " hosts)))
    (when (not (string-empty-p chosen-host))
      (ssh-tramp chosen-host))))

(defun ssh-config-list-hosts (config-file)
  "List hosts defined in the SSH config file."
  (with-temp-buffer
    (insert-file-contents config-file)
    (goto-char (point-min))
    (let (hosts)
      (while (re-search-forward "^Host[[:space:]]+\\([^#[:space:]]+\\)" nil t)
        (push (match-string 1) hosts))
      hosts)))

(defun ssh-tramp (host)
  "Open an SSH connection to HOST."
  ;; (shell)
  ;; (insert (format "ssh %s\n" host))
  ;; (comint-send-input))
  (let ((tramp-file-name (concat "/ssh:" host "|sudo:" host ":/tmp") ))
    (find-file tramp-file-name)))

(defun ssh-interactive (host)
  "Open an SSH connection to HOST."
  ;; (shell)
  ;; (insert (format "ssh %s\n" host))
  ;; (comint-send-input))
  (interactive "sHost: \nsCommand: ")
  (let ((tramp-file-name (format "/ssh:%s|sudo:%s|/tmp" host host)))
    (find-file tramp-file-name)))

(defun ssh-exec-command (host command)
  "Execute a command on a remote host via SSH."
  (interactive "sHost: \nsCommand: ")
  (let* ((tramp-file-name (format "/ssh:%s|sudo:%s:/tmp" host host))
         (default-directory tramp-file-name)
         (output-buffer (get-buffer-create "*ssh-output*")))
    (with-current-buffer output-buffer
      (erase-buffer))
    (tramp-handle-shell-command command output-buffer output-buffer)
    (display-buffer output-buffer)))


(defun gnuplot-rectangle (&optional title style)
  (interactive)
  (let* ((name (make-temp-file "plot"))
         (buf (find-file name))
         xlabel ylabel cols header n)
    (with-current-buffer buf
      (setq cols (split-string (car killed-rectangle)))
      (when (string-match-p "^[a-zA-Z]" (car cols))
        (setq header cols)
        (pop killed-rectangle))
      (setq n (length header))
      (pcase n
        (1 (setq ylabel (nth 1 header)))
        (2 (setq xlabel (nth 0 header)
                 ylabel (nth 1 header)))
        (_ (setq xlabel (nth 0 header))))
      (yank-rectangle)
      (save-buffer))

    (setq style (or style "line"))
    (with-temp-buffer
      (insert "set title  '" (or title "Title") "'\n")
      (insert "set xlabel '" (or xlabel "x-axis") "'\n")
      (insert "set ylabel '" (or ylabel "y-axis") "'\n")
      (insert "plot '" name "'")
      (setq n (length cols))
      (dotimes (i (1- n))
        (if (> n 1) (insert " using 1:" (number-to-string (+ i 2))))
        (if (< i n) (insert " with " style))
        (if (> n 2)
            (insert " title '" (nth (+ i 1) header) "'")
          (insert " notitle"))
        (if (> i 0) (insert ",")))
      (if (= 1 n) (insert " using 1 with " style " notitle"))
      (newline)
      (gnuplot-mode)
      (gnuplot-send-buffer-to-gnuplot))

    ;; Cleanup
    (kill-buffer buf)
    ;; (delete-file name)
    ))


(use-package auctex
  :ensure t
  :hook
  (LaTeX-mode . turn-on-prettify-symbols-mode)
  (LaTeX-mode . turn-on-flyspell))

;; (use-package vertico-posframe
;;   :ensure t
;;   :config (vertico-posframe-mode nil))
