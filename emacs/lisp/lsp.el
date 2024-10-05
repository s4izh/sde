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
  "ln" 'lsp-ui-find-next-reference
  "lp" 'lsp-ui-find-prev-reference
  "lw" 'lsp-ivy-workspace-symbol
  "ls" 'counsel-imenu
  "le" 'lsp-ui-flycheck-list
  "lS" 'lsp-ui-sideline-mode
  "la" 'lsp-execute-code-action)


(setq lsp-completion-provider :none)
(defun corfu-lsp-setup ()
  (setq-local completion-styles '(orderless)
              completion-category-defaults nil))
(add-hook 'lsp-mode-hook #'corfu-lsp-setup)
