;;; appearance.el --- visual chrome, scrolling, theme and font -*- lexical-binding: t; -*-

(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

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

(load-theme 'modus-vivendi)
(set-face-attribute 'default nil :font "JetBrains Mono" :height 75)
