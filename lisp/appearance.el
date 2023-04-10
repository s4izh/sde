;; UI
(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)

(menu-bar-mode -1)            ; Disable the menu bar
(setq ring-bell-function 'ignore)
(display-battery-mode t)

(add-to-list 'default-frame-alist '(font . "DejaVuSansMono-12"))
(setq locale-coding-system 'utf-8)

;; THEME
(use-package doom-themes
  :config
  (load-theme 'doom-sourcerer t))

(use-package spaceway-theme
  ;; :disabled t
  :ensure nil
  :load-path "lisp/spaceway/"
  :config
  (global-hl-line-mode t)
  (set-cursor-color "#dc322f")
  (load-theme 'spaceway t))

;; smooth scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-margin '3)
(setq use-dialog-box nil)

;; line numbers

(column-number-mode)
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)
(set-default 'truncate-lines t)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                org-agenda-mode-hook
                compilation-mode-hook
		woman-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
