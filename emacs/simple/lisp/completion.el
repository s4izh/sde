;;; completion.el --- minibuffer and in-buffer completion stack -*- lexical-binding: t; -*-

(use-package vertico
  :init
  (vertico-mode)
  ;; One-chord export of the live candidate list to a persistent, compilation-
  ;; style jump buffer (*grep* for ripgrep/grep, *Occur* for consult-line).
  ;; Navigate it with M-g M-n / M-g M-p like *compilation*. Also on C-. E.
  :bind (:map vertico-map
              ("C-c C-e" . embark-export)))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package orderless
  :custom
  ;; set Orderless as the default completion style
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package corfu
  :custom
  (corfu-auto t)                   ; show popup automatically while typing
  (corfu-auto-delay 0.2)           ; 200ms delay to avoid flicker
  (corfu-auto-prefix 2)            ; start suggesting after 2 characters
  (corfu-quit-no-match 'separator)
  :init
  (global-corfu-mode))

(use-package consult
  :bind (;; C-x — buffers
         ("C-x B"   . consult-buffer)          ; switch buffer w/ preview + extra sources
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x p b" . consult-project-buffer)  ; project buffers
         ;; M-g — goto
         ("M-g g"   . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g i"   . consult-imenu)           ; definition in this buffer
         ("M-g I"   . consult-imenu-multi)     ; across project buffers
         ("M-g m"   . consult-mark)
         ("M-g f"   . consult-flymake)
         ;; M-s — search
         ("M-s l"   . consult-line)            ; find a line in this buffer
         ("M-s L"   . consult-line-multi)      ; across buffers
         ("M-s g"   . consult-grep)
         ("M-s r"   . consult-ripgrep)         ; grep across the project
         ("M-s f"   . consult-find)
         ;; misc
         ("M-y"     . consult-yank-pop)))      ; browse the kill-ring

;; (use-package yasnippet
;;  :init
;;  (yas-global-mode 1))

;; extra package with hundreds of ready-made snippets for C, C++, CMake, etc.
;; (use-package yasnippet-snippets)

(use-package embark
  :bind
  (("C-." . embark-act)))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; TODO: check how to use this
(use-package wgrep
  :custom
  (wgrep-auto-save-buffer t))
