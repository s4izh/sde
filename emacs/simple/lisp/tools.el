;;; tools.el --- general editing/VC tools -*- lexical-binding: t; -*-

(use-package magit
  :config
  :bind (("C-x g" . magit-status)))

(use-package which-key
  :init
  (which-key-mode)
  :custom
  (which-key-idle-delay 1.0))
