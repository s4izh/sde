(setq c-default-style "linux"
  c-basic-offset 4)

(use-package flycheck
  :defer t)

(use-package eglot
  :defer t
  :config
  (setq eglot-sync-connect 0))

;; (use-package rust-ts-mode
;;   :mode ("\\.rs\\'" . rust-ts-mode)
;;   :hook ((rust-ts-mode . flycheck-mode)
;;          (rust-ts-mode . eglot-ensure)))

(use-package rust-mode
  :defer t
  :mode ("\\.rs\\'" . rust-mode)
  :hook ((rust-mode . flycheck-mode)
          (rust-mode . eglot-ensure))
  :init (setq rust-format-on-save t))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package yaml-mode
  :defer t
  :mode "\\.yml\\'")

(use-package zig-mode
  :defer t
  :mode "\\.zig\\'")

(use-package toml-mode
  :defer t)

(use-package json-mode
  :defer t)

(use-package docker
  :bind ("C-c d" . docker))

(use-package dockerfile-mode
  :defer t)

(use-package systemd
  :defer t)

(use-package direnv
  :config
  (direnv-mode))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package bitbake
  :defer t)

(setq treesit-language-source-alist
  '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (c "https://github.com/tree-sitter/tree-sitter-c")
     (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(use-package multi-compile
  :defer t)

(setq multi-compile-alist '(
                             ("*scratch*" . (("print-hello" . "echo 'hello'")
                                              ("otra cosa" . "echo 'otra cosa'")))
                             (rust-mode . (("rust-run" . "cargo run")
                                            ("rust-clippy" . "cargo clippy")
                                            ("rust-check" . "cargo check")
                                            ("rust-test" . "cargo test")
                                            ("rust-release" . "cargo run --release")))
                             ("\\.*" . (("compiler" . "compiler %file-name")))
                             ))

(use-package yasnippet
  :defer t)

(use-package yasnippet-snippets
  :defer t)

(use-package eldoc-box
  :defer t)

(use-package dts-mode
  :defer t)
