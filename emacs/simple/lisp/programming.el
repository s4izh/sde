;;; programming.el --- language tooling -*- lexical-binding: t; -*-

(use-package eglot
  :ensure nil
  :hook
  ((c-mode . eglot-ensure)
   (c++-mode . eglot-ensure))
  :custom
  (eglot-events-buffer-size 0) ; disable LSP server log (avoids RAM bloat)
  (eglot-autoshutdown t)       ; kill clangd when the last file is closed
  (eglot-sync-connect 1)       ; don't freeze the UI if the server takes 1s to start
  (eglot-ignored-server-capabilities '(:inlayHintProvider)) ; no inlay hints
  :config
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode)
                 . ("clangd"
                    "--background-index"        ; index files in the background
                    "--clang-tidy"              ; enable the modern C++ linter
                    "--header-insertion=iwyu"   ; smart header auto-insertion
                    "--completion-style=detailed"
                    "--function-arg-placeholders" ; add argument placeholders on completion
                    "-j=4")))                   ; use 4 threads (raise it if you have more cores)

  ;; Neovim 0.11's default LSP mappings, on `eglot-mode-map' so they only exist
  ;; where a server is attached. Vanilla equivalents (M-. M-, M-?) still work.
  (with-eval-after-load 'evil
    (evil-define-key 'normal eglot-mode-map
      (kbd "grn") 'eglot-rename
      (kbd "gra") 'eglot-code-actions
      (kbd "grr") 'xref-find-references
      (kbd "gri") 'eglot-find-implementation
      (kbd "grt") 'eglot-find-typeDefinition
      (kbd "gO")  'imenu                    ; document symbols
      (kbd "gd")  'xref-find-definitions
      (kbd "gD")  'eglot-find-declaration
      (kbd "gs")  'eldoc                    ; signature/hover in the echo area
      (kbd "K")   'eldoc-doc-buffer)))      ; hover in a window, like nvim's K

(setq-default c-default-style "linux"
              c-basic-offset 4)

(use-package nix-mode
  :mode "\\.nix\\'")

;; Prefer the tree-sitter mode when its grammar is installed; otherwise the
;; nix-mode association above stays in effect. nix-mode has no built-in
;; tree-sitter derivation, so this is a separate package rather than a flag.
(use-package nix-ts-mode
  :when (treesit-language-available-p 'nix)
  :mode "\\.nix\\'")

(use-package rust-mode
  :mode "\\.rs\\'"
  :custom
  (rust-mode-treesitter-derive t)) ; use the tree-sitter grammar when available
