(use-package lsp-mode
  ;; :ensure t
  ;; :hook((lsp-mode . corfu-mode))
  :config
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  (lsp-enable-which-key-integration t))
  ;; :custom (lsp-headerline-breadcrumb-enable nil))

(use-package lsp-ui
  ;;:straight t
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable t)
  ;; (setq lsp-ui-sideline-show-hover nil)
  ;; (setq lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-show))

(ss/leader-key-def
  "l"  '(:ignore t :which-key "lsp")
  "ld" 'xref-find-definitions
  "lr" 'xref-find-references
  ;; "ln" 'lsp-ui-find-next-reference
  ;; "lp" 'lsp-ui-find-prev-reference
  ;; "lw" 'lsp-ivy-workspace-symbol
  ;; "ls" 'counsel-imenu
  ;; "le" 'lsp-ui-flycheck-list
  ;; "lS" 'lsp-ui-sideline-mode
  "la" 'lsp-execute-code-action)

(use-package flycheck)

(use-package rust-mode
  :hook ((rust-mode . flycheck-mode)
         (rust-mode . lsp-deferred))
  :init (setq rust-format-on-save t))

(setq lsp-completion-provider :none)
(defun corfu-lsp-setup ()
  (setq-local completion-styles '(orderless)
              completion-category-defaults nil))
(add-hook 'lsp-mode-hook #'corfu-lsp-setup)

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package yaml-mode
  :mode "\\.yml\\'")

(use-package docker
  :bind ("C-c d" . docker))

(use-package dockerfile-mode)

(use-package systemd)

(use-package direnv
  :config
  (direnv-mode))

(use-package editorconfig
  :config
  (editorconfig-mode 1))


(use-package bitbake)
