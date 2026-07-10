;;; init.el --- new and clean emacs config

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

(global-display-line-numbers-mode 1) ; mostrar números de línea en el margen
(column-number-mode)                 ; mostrar el número de columna abajo
(setq make-backup-files nil)         ; no llenar las carpetas de archivos terminados en ~
(setq auto-save-default nil)         ; no crear archivos de autoguardado #molestos#

(set-face-attribute 'default nil :font "JetBrains Mono" :height 75)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  ;; configura Orderless como el motor de búsqueda por defecto
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))
