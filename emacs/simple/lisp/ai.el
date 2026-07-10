;;; ai.el --- AI assistant integration -*- lexical-binding: t; -*-

;; agent-shell isn't on MELPA, so it's installed via package-vc.

(unless (package-installed-p 'agent-shell)
  (package-vc-install "https://github.com/xenodium/agent-shell"))

(use-package agent-shell
  :ensure nil
  :bind (("C-c a" . agent-shell)))
