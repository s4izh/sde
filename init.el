;; -*- lexical-binding: t; -*-

;; This file bootstraps the configuration, which is divided into
;; a number of other files inside the `lisp' directory.

(setq user-emacs-directory "~/.config/emacs/")

(setq ss/is-guix
      (string-suffix-p "This is the GNU system.  Welcome."
		       (string-trim (shell-command-to-string "cat /etc/issue"))))

(setq ss/is-linux (eq system-type 'gnu/linux))

(setq package-archives
  '(("melpa" . "https://melpa.org/packages/")
    ("org" . "https://orgmode.org/elpa/")
    ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

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

(load (concat user-emacs-directory
              "lisp/evil.el"))

(use-package async
  :ensure t
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
(use-package no-littering
  :ensure t)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(setq custom-file (locate-user-emacs-file "custom-vars.el"))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t))) (load custom-file 'noerror 'nomessage)

;; (load (concat user-emacs-directory
;;         "lisp/modeline.el"))

(load (concat user-emacs-directory
	      "lisp/appearance.el"))


(use-package dired
  :ensure nil
  :commands (dired)
  :hook ((dired-mode . hl-line-mode)
         (dired-mode . dired-omit-mode)
         (dired-mode . dired-hide-details-mode))
  :bind (:map dired-mode-map
              ("-" . dired-up-directory))
  :init

  (setq dired-bind-jump nil)
  :config
  (setq dired-listing-switches "-aghoA --group-directories-first")
  ;;;;; Hide . and .. in dired
  (setq dired-omit-files
        (setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..*$"))
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
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;;; parenthesis
(use-package smartparens
  :hook (prog-mode . smartparens-mode))

(use-package rainbow-delimiters
  :init
  :hook (prog-mode . rainbow-delimiters-mode))

;; remove trailing whitespaces
(use-package ws-butler
  :ensure t
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

(load (concat user-emacs-directory
              "lisp/general.el"))

(load (concat user-emacs-directory
              "lisp/org.el"))

;; (load (concat user-emacs-directory
;;               "lisp/ai.el"))

(load (concat user-emacs-directory
              "lisp/ai2.el"))

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
              ("<return>" . corfu-complete)
              ;; ("C-y" . corfu-complete) ;; doesnt work idk why
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

(use-package consult
  :after vertico
  :bind (("C-x b"       . consult-buffer))
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
         ;; ("C-x M-:"     . consult-complex-command)
         ;; ("C-c n"       . consult-org-agenda)
         ;; ("C-c m"       . my/notegrep)
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

(use-package embark
  :bind(("C-," . embark-act)))

;;; PASS
;; (use-package password-store
;;   :commands (password-store-copy
;;              password-store-insert
;;              password-store-generate))

;; Authenticte with auth-source-pass
;; (use-package auth-source-pass
;;   :after password-store
;;   :config
;;   (auth-source-pass-enable))

;; (load (concat user-emacs-directory
;;               "lisp/guix.el"))

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))

(defun sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))
