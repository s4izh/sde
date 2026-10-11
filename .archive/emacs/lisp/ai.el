;;; ai.el --- AI tooling -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package agent-shell
  :ensure t
  :config
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t)))
