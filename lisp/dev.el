(use-package lsp-mode
  :ensure t
  :hook((lsp-mode . corfu-mode))
  :config
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  (lsp-enable-which-key-integration t)
  :custom (lsp-headerline-breadcrumb-enable nil))

(use-package rust-mode
  :ensure t
  :hook ((rust-mode . flycheck-mode)
         (rust-mode . lsp-deferred))
  :init (setq rust-format-on-save t))

(setq lsp-completion-provider :none)
(defun corfu-lsp-setup ()
  (setq-local completion-styles '(orderless)
              completion-category-defaults nil))
(add-hook 'lsp-mode-hook #'corfu-lsp-setup)
