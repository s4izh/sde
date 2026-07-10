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

(load-theme 'modus-vivendi)
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

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)                   ; Mostrar popup automáticamente al escribir
  (corfu-auto-delay 0.2)           ; Retraso de 200ms para no parpadear locamente
  (corfu-auto-prefix 2)            ; Empezar a sugerir tras 2 caracteres
  (corfu-quit-no-match 'separator)
  :init
  (global-corfu-mode))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))) ; Atajo global para abrir Magit

(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  :custom
  (which-key-idle-delay 1.0))

(use-package eglot
  :ensure nil ; Eglot es nativo en Emacs 29+, no lo descargamos de MELPA
  :hook
  ;; Activar Eglot automáticamente al abrir archivos C y C++
  ((c-mode . eglot-ensure)
   (c++-mode . eglot-ensure))
  :custom
  ;; OPTIMIZACIONES CRÍTICAS DE RENDIMIENTO
  (eglot-events-buffer-size 0) ; Desactivar el log del servidor LSP (evita bloqueos de RAM)
  (eglot-autoshutdown t)       ; Matar el proceso clangd al cerrar el último archivo
  (eglot-sync-connect 1)       ; No congelar la UI si el servidor tarda 1 seg en arrancar
  :config
  ;; Parámetros de línea de comandos para hacer que Clangd vuele en C++
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode)
                 . ("clangd"
                    "--background-index"        ; Indexar archivos en segundo plano
                    "--clang-tidy"              ; Activar el linter moderno de C++
                    "--header-insertion=iwyu"   ; Auto-importar cabeceras inteligentemente
                    "--completion-style=detailed"
                    "--function-arg-placeholders" ; Poner placeholders en los argumentos al autocompletar
                    "-j=4"))))                  ; Usar 4 hilos del procesador (súbelo si tienes más cores)


(use-package consult
  :ensure t
  ;; Remapeamos algunas funciones nativas de Emacs para que usen la versión dopada de Consult
  :bind (("C-x b" . consult-buffer)      ; Mejor cambio de buffers
         ("C-s"   . consult-line)        ; Mejor búsqueda en el archivo
         ("M-g i" . consult-imenu)       ; Saltar a funciones/clases del archivo
         ("M-s r" . consult-ripgrep)))   ; Buscar texto en todo el proyecto (requiere tener 'rg' instalado en tu SO)

;; ==========================================
;; 11. MOTOR DE SNIPPETS (YASNIPPET)
;; ==========================================
;; (use-package yasnippet
;;  :ensure t
;;  :init
;;  (yas-global-mode 1))

;; Paquete extra con cientos de snippets ya hechos para C, C++, CMake, etc.
;; (use-package yasnippet-snippets
;;  :ensure t)

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act))) ; Atajo universal para decir "¿Qué puedo hacer con esto?"

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep
  :ensure t
  :custom
  (wgrep-auto-save-buffer t))
