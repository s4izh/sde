;;; evil.el --- evil mode -*- no-byte-compile: t; lexical-binding: t; -*-

;; evil-want-keybinding must be declared before Evil and Evil Collection
(setq evil-want-keybinding nil)

(use-package evil
  :ensure t
  :init
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :custom
  (evil-want-Y-yank-to-eol t)
  :config
  (evil-select-search-module 'evil-search-module 'evil-search)
  ;; (defun my-evil-scroll-up-and-center ()
  ;;   "Scroll the buffer up a half-window and center the cursor."
  ;;   (interactive)
  ;;   (scroll-up-command) ; Scroll up by half the window height
  ;;   (recenter))            ; Center the cursor in the window
  ;; 
  ;; (defun my-evil-scroll-down-and-center ()
  ;;   "Scroll the buffer down a half-window and center the cursor."
  ;;   (interactive)
  ;;   (scroll-down-command)          ; Scroll down by half the window height
  ;;   (recenter))            ; Center the cursor in the window
  ;; 
  ;; (define-key evil-normal-state-map (kbd "C-u") 'my-evil-scroll-up-and-center)
  ;; (define-key evil-visual-state-map (kbd "C-u") 'my-evil-scroll-up-and-center)
  ;; 
  ;; (define-key evil-normal-state-map (kbd "C-d") 'my-evil-scroll-down-and-center)
  ;; (define-key evil-visual-state-map (kbd "C-d") 'my-evil-scroll-down-and-center)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package undo-fu
  :ensure t
  :commands (undo-fu-only-undo
             undo-fu-only-redo
             undo-fu-only-redo-all
             undo-fu-disable-checkpoint)
  :custom
  ;; 3 times the default values
  (undo-limit (* 3 160000))
  (undo-strong-limit (* 3 240000)))

(use-package undo-fu-session
  :ensure t
  :config
  (undo-fu-session-global-mode))

;; replaces evil-commentary packages
(with-eval-after-load "evil"
  (evil-define-operator my-evil-comment-or-uncomment (beg end)
    "Toggle comment for the region between BEG and END."
    (interactive "<r>")
    (comment-or-uncomment-region beg end))
  (evil-define-key 'normal 'global (kbd "gc") 'my-evil-comment-or-uncomment))

;; (use-package vim-tab-bar
;;   :ensure t
;;   :commands vim-tab-bar-mode
;;   :hook (after-init . vim-tab-bar-mode))
