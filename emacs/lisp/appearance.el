;;; appearance.el --- basic appearance settings -*- no-byte-compile: t; lexical-binding: t; -*-

(set-face-attribute 'default nil
                    :family "monospace"
                    :height 140)

(use-package doom-themes
  :defer t)

(use-package gruber-darker-theme :defer t)
(use-package naysayer-theme :defer t)

(use-package ef-themes :defer t)

(use-package modus-themes :defer t)

(use-package spaceway-theme
  ;; :disabled t
  :ensure nil
  :load-path "lisp/spaceway/"
  :config ())
  ;; (global-hl-line-mode t)
  ;; (set-cursor-color "#dc322f"))
  ;; (load-theme 'spaceway t))

(setq my/current-theme 'tango-dark)

(defun my/load-theme (theme)
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar #'symbol-name
                                     (custom-available-themes))))))
  (disable-theme my/current-theme)
  (setq my/current-theme theme)
  (load-theme theme t))

(my/load-theme my/current-theme)

;; (use-package nerd-icons
;;   :ensure t)

;; (use-package nerd-icons-corfu
;;   :ensure t
;;   :after corfu
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; (use-package mood-line)
