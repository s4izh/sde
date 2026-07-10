;;; completion.el --- minibuffer and in-buffer completion stack -*- lexical-binding: t; -*-

(use-package vertico
  :init
  (vertico-mode))

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
  :bind (;("C-x b" . consult-buffer)
         ;("C-s"   . consult-line)
         ("M-g i" . consult-imenu)
         ("M-s r" . consult-ripgrep)))

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
