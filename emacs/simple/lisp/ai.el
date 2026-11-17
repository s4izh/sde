;;; ai.el --- AI assistant integration -*- lexical-binding: t; -*-

(use-package agent-shell
  :bind (("C-c a" . agent-shell)
         ("C-c A" . agent-shell-pi-start-agent))
  :custom
  ;; Keep the agent picker, with pi preselected.
  (agent-shell-preferred-agent-config '(preselect . pi))
  ;; pi-acp (global npm, ~/.npm/bin) spawns `pi --mode rpc'. It finds the
  ;; ~/.local/scripts/pi wrapper on PATH, so Emacs and the terminal run the
  ;; same pi with the same ~/.pi/agent config (the toolbox harness). The
  ;; gepeto key is read by pi itself from ~/.secrets, so Emacs does not need
  ;; GEPETO_API_KEY in its environment.
  (agent-shell-pi-acp-command '("pi-acp")))

(use-package pilish
  :defer t)
