;;; appearance.el --- visual chrome, scrolling, theme and font -*- lexical-binding: t; -*-

;; Frame chrome (menu/tool/scroll bars), the default font and
;; `inhibit-startup-message' are handled in early-init.el, before the first
;; frame is created. `tooltip-mode' is not a frame parameter, so it stays here.
(tooltip-mode -1)

(global-display-line-numbers-mode 0) ; show line numbers in the margin
(column-number-mode)                 ; show column number in the mode line

;; native emacs smooth scrolling
(pixel-scroll-precision-mode 1)

(setq scroll-margin 6)
(setq scroll-conservatively 101) ;; only 1 line scroll
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)

(setq-default display-line-numbers-width 4)
(setq-default display-line-numbers-grow-only t)

(use-package spaceway-theme
  :ensure nil
  :load-path "lisp/spaceway/"
  :config ())

(use-package spaceway-light-theme
  :ensure nil
  :load-path "lisp/spaceway/"
  :config ())

(use-package ef-themes
  :defer t)

(setq ssm/current-theme 'ef-elea-dark)

(defun ssm/load-theme (theme)
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar #'symbol-name
                                     (custom-available-themes))))))
  (disable-theme ssm/current-theme)
  (setq ssm/current-theme theme)
  (load-theme theme t))

(ssm/load-theme ssm/current-theme)
