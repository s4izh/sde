;;; post-init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package compile-angel
  :ensure t
  :demand t
  :config
  (compile-angel-on-load-mode)
  (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode))

(load (concat user-emacs-directory
              "lisp/evil.el"))

(load (concat user-emacs-directory
              "lisp/appearance.el"))

(use-package eglot
  :ensure nil
  :defer t
  :commands (eglot
             eglot-rename
             eglot-ensure
             eglot-rename
             eglot-format-buffer)

  :custom
  (eglot-report-progress nil)  ; Prevent minibuffer spam

  :config
  ;; Optimizations
  ;; (setq eglot-ignored-server-capabilities '(:inlayHintProvider))
  (fset #'jsonrpc--log-event #'ignore)
  (setq jsonrpc-event-hook nil))

(use-package eldoc
  :ensure nil
  :defer t
  :config
  (setq eldoc-echo-area-use-multiline-p nil))

(use-package corfu
  :ensure t
  :defer t
  :commands (corfu-mode global-corfu-mode)

  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))

  :custom
  ;; Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Disable Ispell completion function. As an alternative try `cape-dict'.
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)

  ;; Enable Corfu
  :config
  (global-corfu-mode))

(use-package cape
  :ensure t
  :defer t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

;; Hide warnings and display only errors
(setq warning-minimum-level :error)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(pixel-scroll-precision-mode)

(display-time-mode)
(show-paren-mode +1)  ; Paren match highlighting
(winner-mode 1)
(pixel-scroll-precision-mode 1)

;; Configure Emacs to ask for confirmation before exiting
(setq confirm-kill-emacs 'y-or-n-p)

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator "•")
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))

;; Window dividers separate windows visually. Window dividers are bars that can
;; be dragged with the mouse, thus allowing you to easily resize adjacent
;; windows.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Window-Dividers.html
(add-hook 'after-init-hook #'window-divider-mode)

(use-package dired
  :ensure nil
  :commands (dired)
  ;; :hook ((dired-mode . hl-line-mode)
  ;;        (dired-mode . dired-omit-mode)
  ;;        (dired-mode . dired-hide-details-mode))
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


;; vertico consult embark ----------------

;; Tip: You can remove the `vertico-mode' use-package and replace it
;;      with the built-in `fido-vertical-mode'.
(use-package vertico
  ;; (Note: It is recommended to also enable the savehist package.)
  :ensure t
  :defer t
  :commands vertico-mode
  :hook (after-init . vertico-mode))

(use-package orderless
  ;; Vertico leverages Orderless' flexible matching capabilities, allowing users
  ;; to input multiple patterns separated by spaces, which Orderless then
  ;; matches in any order against the candidates.
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  ;; Marginalia allows Embark to offer you preconfigured actions in more contexts.
  ;; In addition to that, Marginalia also enhances Vertico by adding rich
  ;; annotations to the completion candidates displayed in Vertico's interface.
  :ensure t
  :defer t
  :commands (marginalia-mode marginalia-cycle)
  :hook (after-init . marginalia-mode))

(use-package embark
  ;; Embark is an Emacs package that acts like a context menu, allowing
  ;; users to perform context-sensitive actions on selected items
  ;; directly from the completion interface.
  :ensure t
  :defer t
  :commands (embark-act
             embark-dwim
             embark-export
             embark-collect
             embark-bindings
             embark-prefix-help-command)
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :ensure t
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)
         ;; ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))

  ;; Enable automatic preview at point in the *Completions* buffer.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Optionally configure the register formatting. This improves the register
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))

;; ----------------- vertico consult embark

(use-package envrc
  :ensure t
  :defer t
  :hook (after-init . envrc-global-mode))

;; compilation mode
(use-package compilation-mode
  :ensure nil
  :defer t
  :config
  (require 'ansi-color)
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)
  (setopt compilation-ask-about-save nil))

(use-package ansi-color
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter)) 

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (when (eq major-mode 'compilation-mode)
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(defun generic-compiler ()
  (concat "compiler "
          (if buffer-file-name
              (shell-quote-argument buffer-file-name))))

(defvar custom-compiler-modes
  `((purescript-mode . "spago run")
    (vue-ts-mode    . "npx eslint --fix . && npx vue-tsc --noEmit")))

(defun get-compiler ()
  (let* ((compiler (assoc-default major-mode
                                  custom-compiler-modes
                                  'eql nil)))
    (cond ((or (file-exists-p "makefile")
               (file-exists-p "Makefile"))
           "make -k ")
          ((functionp compiler) (funcall compiler))
          ((stringp compiler) compiler)
          (t (funcall #'generic-compiler)))))

(defun run-custom-compile ()
  "Run a custom compiler command determined by `get-compiler`."
  (interactive)
  (let ((compile-command (get-compiler)))
    (call-interactively 'compile)))

(use-package nix-mode
  :ensure t
  :defer t)

(use-package project
  :ensure nil
  :defer t
  :config
  (setq project-vc-extra-root-markers '(".projectile")))

(setq grep-command "grep --color=auto -nHr ")

(use-package pdf-tools
  :config
  (pdf-tools-install))

(defun sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))


(use-package perspective
  :bind
  ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))  ; pick your own prefix key here
  :init
  (persp-mode))

(use-package markdown-mode
  :ensure t
  :defer t)

(use-package wgrep
  :defer t)

(setq xref-show-xrefs-function #'xref-show-definitions-buffer)

(use-package vterm
  :defer t)

(defun ss/eval-and-show-inline ()
  "Evaluate the expression at point and display the result next to it."
  (interactive)
  (let ((expr (thing-at-point 'line t)))
    (when expr
      (let ((result (eval (read expr))))
        (end-of-line)
        (insert " ;; => " (prin1-to-string result))))))

(defvar ss/my-eval-overlays nil
  "List of overlays for showing evaluation results.")

(defun ss/eval-and-show-virtual-text ()
  "Evaluate the expression at point and display the result as virtual text."
  (interactive)
  (let ((expr (thing-at-point 'line t)))  ;; Get the expression at point
    (when expr
      (let* ((result (eval (read expr)))  ;; Evaluate the expression
             (result-str (prin1-to-string result))  ;; Convert result to string
             (overlay (make-overlay (line-end-position) (line-end-position))))  ;; Create overlay after the expression
        ;; Set overlay to show the result with custom styling
        (overlay-put overlay 'after-string (concat " => " result-str))
        ;; (overlay-put overlay 'face '(:foreground "blue" :weight bold))  ;; Apply styling to the virtual text
        ;; Store the overlay in a list to remove later
        (push overlay my-eval-overlays)))))

(defun ss/remove-all-eval-overlays ()
  "Remove all displayed evaluation overlays."
  (interactive)
  (dolist (overlay my-eval-overlays)
    (delete-overlay overlay))  ;; Remove each overlay
  (setq my-eval-overlays nil))  ;; Clear the list of overlays

(defun eval-and-copy-to-clipboard ()
  "Evaluate the expression at point, then copy the result to the clipboard."
  (interactive)
  (let ((expr (thing-at-point 'line t)))  ;; Get the expression at point
    (when expr
      (let* ((result (eval (read expr)))  ;; Evaluate the expression
             (result-str (prin1-to-string result)))  ;; Convert result to string
        (kill-new result-str)  ;; Copy result to the clipboard
        (message "Result copied to clipboard: %s" result-str)))))  ;; Display confirmation message

(use-package obsidian
  :ensure t
  :config
  (global-obsidian-mode t)
  (obsidian-backlinks-mode t)
  :custom
  ;; location of obsidian vault
  (obsidian-directory "~/notes")
  ;; Default location for new notes from `obsidian-capture'
  (obsidian-inbox-directory "inbox")
  ;; Useful if you're going to be using wiki links
  (markdown-enable-wiki-links nil)

  ;; These bindings are only suggestions; it's okay to use other bindings
  :bind (:map obsidian-mode-map
              ;; Create note
              ("C-c C-n" . obsidian-capture)
              ;; If you prefer you can use `obsidian-insert-wikilink'
              ("C-c C-l" . obsidian-insert-link)
              ;; Open file pointed to by link at point
              ("C-c C-o" . obsidian-follow-link-at-point)
              ;; Open a different note from vault
              ("C-c C-p" . obsidian-jump)
              ;; Follow a backlink for the current file
              ("C-c C-b" . obsidian-backlink-jump)))

<<<<<<< Updated upstream
(use-package magit
  :ensure t
  :defer t)

(use-package rust-mode
  :ensure t
  :defer t)

(use-package lua-mode
  :ensure t
  :defer t)

(use-package rmsbolt
  :ensure t
  :defer t)

(use-package general
  :ensure t
  :config
  (general-create-definer my/leader
    :prefix "SPC"))

(my/leader
  :keymaps 'normal
  "p"  '(:ignore t :which-key "project")
  "p f" 'project-find-file
  "p F" 'project-find-regexp
  "p b" 'project-switch-to-buffer
  "p d" 'project-dired
  "p e" 'project-eshell
  "p s" 'project-shell
  "p c" 'project-compile
  "p r" 'project-recompile
  "p p" 'project-switch-project
  "p k" 'project-kill-buffers)

(use-package org-embed
  :ensure nil
  :load-path "lisp/org-embed")
(defun ethan/append-to-list (list-var elements)
  "Append ELEMENTS to the end of LIST-VAR.
	The return value is the new value of LIST-VAR."
  (unless (consp elements)
    (error "ELEMENTS must be a list"))
  (let ((list (symbol-value list-var)))
    (if list
	(setcdr (last list) elements)
      (set list-var elements)))
  (symbol-value list-var))

(use-package mixed-pitch
  :ensure t
  :hook((LaTeX-mode . mixed-pitch-mode)
	      (org-mode . mixed-pitch-mode))
  :config
  (ethan/append-to-list 'mixed-pitch-fixed-pitch-faces
			'(solaire-line-number-face
			  org-date
			  org-footnote
			  org-special-keyword
			  org-property-value
			  org-ref-cite-face
			  org-tag
			  org-todo-keyword-todo
			  org-todo-keyword-habt
			  org-todo-keyword-done
			  org-todo-keyword-wait
			  org-todo-keyword-kill
			  org-todo-keyword-outd
			  org-todo
			  org-done
			  org-modern-priority
			  org-modern-tag
			  org-modern-done
			  org-modern-date-active
			  org-modern-date-inactive
			  org-modern-time-active
			  org-modern-time-inactive
			  org-drawer
			  font-lock-comment-face
			  )))

(use-package fontaine
  :ensure t
  :config
  ;; The concise one which relies on "implicit fallback values"
  (setq fontaine-presets
	'((regular
	   :default-height 100)
	  ;; settinging some font for a smaller screen
	  (small-screen
	   :default-weight semilight
	   :default-height 140)
	  ;; settinging some font for a larger screen
	  (larger-screen
	   :default-weight semilight
	   :default-height 155)
	  (large
	   :default-weight semilight
	   :default-height 180
	   :bold-weight extrabold)
	  (t ; our shared fallback properties
	   :default-family "monospace"
	   :default-weight medium
	   ;; I just really like computer modern font
	   :variable-pitch-family "CMU Serif"
	   :variable-pitch-height 1.5)))
  ;; now set the preset
  ;; if is my laptop or lab machine, the screens are a bit smaller and I am sitting
  ;; a bit closer so will make the font a tad larger
  ;; you can simplify this on your end if you want, just need to call
  ;; `(fontaine-set-preset 'VALUE) 
  ;; with whatever you called your display setting
  (if (or (equal (system-name) "lab") (equal (system-name) "mover"))
      (fontaine-set-preset 'small-screen)
    ;; otherwise at home with a screen that is a touch bigger
    ;; and am sitting a bit further away so make font bigger
    (fontaine-set-preset 'larger-screen)))

(global-set-key (kbd "<mouse-9>") 'next-buffer)
(global-set-key (kbd "<mouse-8>") 'previous-buffer)
=======
;; Create note
(define-key obsidian-mode-map (kbd "C-c o c") 'obsidian-capture)
;; If you prefer you can use `obsidian-insert-wikilink'
(define-key obsidian-mode-map (kbd "C-c o l") 'obsidian-insert-link)
;; Open file pointed to by link at point
(define-key obsidian-mode-map (kbd "C-c C-o") 'obsidian-follow-link-at-point)
;; Open a note note from vault
(define-key obsidian-mode-map (kbd "C-c o j") 'obsidian-jump)
;; Follow a backlink for the current file
(define-key obsidian-mode-map (kbd "C-c o b") 'obsidian-backlink-jump)

>>>>>>> Stashed changes
