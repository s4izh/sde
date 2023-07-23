(global-set-key (kbd "C-M-u") 'universal-argument)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package evil
  :demand t
  ;; :bind (("<escape>" . keyboard-escape-quit))
  :init
  (setq evil-search-module 'evil-search)
  (setq evil-want-keybinding nil)
  ;; (setq evil-want-minibuffer t)
  (setq evil-undo-system 'undo-fu)
  ;;(setq evil-undo-system 'undo-tree)
  (setq evil-want-C-u-scroll t)
  (setq evil-respect-visual-line-mode t) ;; basically gj gk etc
  (setq evil-want-Y-yank-to-eol t)
  :config
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-normal-state-map (kbd "<escape>") 'abort-minibuffers)
  (evil-define-key 'normal 'global "K" 'man)
  ;; (define-key evil-normal-state-map (kbd "/") 'consult-line)
  ;; (define-key evil-normal-state-map (kbd "?") 'consult-line)
  ;; (define-key evil-insert-state-map (kbd "C-n") 'vertico-next)
  ;; (define-key evil-insert-state-map (kbd "C-p") 'vertico-previous)
  ;; (define-key evil-insert-state-map (kbd "C-y") 'corfu-complete)
  (evil-mode 1))


(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "C-y") nil)
  (define-key evil-visual-state-map (kbd "C-y") nil)
  (define-key evil-insert-state-map (kbd "C-y") nil)
  (define-key evil-motion-state-map (kbd "C-y") nil))
  ;; (define-key minibuffer-local-completion-map (kbd "C-n") 'vertico-next))
  ;; (define-key minibuffer-local-map (kbd "C-p") 'vertico-previous))

(use-package evil-collection
  :after evil
  :config
  (setq evil-want-integration t)
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :bind (:map evil-normal-state-map
              ("gc" . evil-commentary)))

(use-package repeat
  ;; :ensure t
  :defer 10
  :init
  (repeat-mode +1))

;; tab widths
(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)
;; spaces instead of tabs
(setq-default indent-tabs-mode nil)
