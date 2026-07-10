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
  :config
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode)
                 . ("clangd"
                    "--background-index"        ; index files in the background
                    "--clang-tidy"              ; enable the modern C++ linter
                    "--header-insertion=iwyu"   ; smart header auto-insertion
                    "--completion-style=detailed"
                    "--function-arg-placeholders" ; add argument placeholders on completion
                    "-j=4"))))                  ; use 4 threads (raise it if you have more cores)

(setq-default c-default-style "linux"
              c-basic-offset 4)
