;; UI
(setq inhibit-startup-message t)

;; (scroll-bar-mode -1)        ; Disable visible scrollbar
;; (tool-bar-mode -1)          ; Disable the toolbar
;; (tooltip-mode -1)           ; Disable tooltips
;; (set-fringe-mode 10)

;; (menu-bar-mode -1)            ; Disable the menu bar
(setq ring-bell-function 'ignore)
(display-battery-mode t)

;; (add-to-list 'default-frame-alist '(font . "DejaVuSansMono-12"))
;; (add-to-list 'default-frame-alist '(font . "LiberationMono-13"))
;; (add-to-list 'default-frame-alist '(font . "FantasqueSansM Nerd Font Mono-15"))
;; (add-to-list 'default-frame-alist '(font . "Ubuntu Mono-14")) ;; ubuntu
;; (add-to-list 'default-frame-alist '(font . "UbuntuMono Nerd Font-14")) ;; arch
;; (add-to-list 'default-frame-alist '(font . "Iosevka Nerd Font Mono-12")) ;; arch
;; (add-to-list 'default-frame-alist '(font . "LiterationMono Nerd Font-12"))
;; (add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font-12"))

(if my/is-ubuntu
    ;; (add-to-list 'default-frame-alist '(font . "JetBrains Mono-12"))
    (add-to-list 'default-frame-alist '(font . "Liberation Mono-12"))
  )

(if my/is-arch
    ;; (add-to-list 'default-frame-alist '(font . "LiterationMono Nerd Font-12"))
    ;; (add-to-list 'default-frame-alist '(font . "Iosevka Nerd Font Mono-13")) ;; arch
    (add-to-list 'default-frame-alist '(font . "FiraCode Nerd Font-12")) ;; arch
    ;; (add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font-12")) ;; arch
)
    ;; (add-to-list 'default-frame-alist '(font . "Iosevka Nerd Font Mono-12")) ;; arch
;; (add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font-12")) ;; arch

(setq locale-coding-system 'utf-8)
(set-face-attribute 'default nil :height 120)

;; THEMES
;; modus
(setq ;;modus-themes-mode-line '(accented borderless)
 modus-themes-bold-constructs t
 modus-themes-italic-constructs t
 modus-themes-mode-line t
 ;; modus-themes-fringes 'subtle
 modus-themes-tabs-accented t
 modus-themes-paren-match '(bold intense)
 ;; modus-themes-prompts '(bold intense)
 modus-themes-completions 'opinionated
 modus-themes-org-blocks 'gray-background
 ;; modus-themes-scale-headings t
 modus-themes-region '(bg-only))

;; (load-theme 'modus-operandi)

(use-package doom-themes
  :defer t)

(use-package spaceway-theme
  ;; :disabled t
  :ensure nil
  :load-path "lisp/spaceway/"
  :config
  ;; (global-hl-line-mode t)
  (set-cursor-color "#dc322f"))
  ;; (load-theme 'spaceway t))

(use-package ef-themes
  :defer t)

(use-package gruber-darker-theme)

(setq my/current-theme 'gruber-darker)
(setq custom-safe-themes t)

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
;; line highlight
(global-hl-line-mode t)

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
                dired-mode-hook
                pdf-view-mode-hook
                woman-mode-hook
                eww-mode-hook
                message-mode-hook
                treemacs-mode-hook
                xref--xref-buffer-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(dolist (mode '(special-mode-hook
                message-mode-hook))
  (add-hook mode (lambda () (visual-line-mode 0))))


;; run nerd-icons-install-fonts
(use-package doom-modeline
  :disabled t
  :config
  (setq doom-modeline-height 22)
  :init (doom-modeline-mode 1))

(use-package mood-line
  :config
  (mood-line-mode))

;; (use-package color-theme-modern)

;; (use-package keycast)

;; (use-package highlight-indent-guides
;;     :config
;;     (setq highlight-indent-guides-method 'character))
