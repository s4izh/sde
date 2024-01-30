(global-set-key (kbd "C-M-u") 'universal-argument)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(defun my/evil-scroll-down-and-center ()
  "Scroll and center the buffer"
  (interactive)
  (evil-scroll-down evil-scroll-count)
  (recenter))

(defun my/evil-scroll-up-and-center ()
  "Scroll down and center the buffer"
  (interactive)
  (evil-scroll-up evil-scroll-count)
  (recenter))

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
  (setq evil-want-C-i-jump nil)
  :config
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-normal-state-map (kbd "<escape>") 'abort-minibuffers)
  (evil-define-key 'normal 'global "K" 'man)
  (define-key evil-normal-state-map (kbd "C-d") 'my/evil-scroll-down-and-center)
  (define-key evil-visual-state-map (kbd "C-d") 'my/evil-scroll-down-and-center)
  (define-key evil-normal-state-map (kbd "C-u") 'my/evil-scroll-up-and-center)
  (define-key evil-visual-state-map (kbd "C-u") 'my/evil-scroll-up-and-center)
  ;; (define-key evil-normal-state-map (kbd "/") 'consult-line)
  ;; (define-key evil-normal-state-map (kbd "?") 'consult-line)
  ;; (define-key evil-insert-state-map (kbd "C-n") 'vertico-next)
  ;; (define-key evil-insert-state-map (kbd "C-p") 'vertico-previous)
  ;; (define-key evil-insert-state-map (kbd "C-y") 'corfu-complete)
  (evil-mode 1))


(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "H") 'previous-buffer)
  (define-key evil-normal-state-map (kbd "L") 'next-buffer)
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

(use-package evil-god-state
  :after evil
  :config
  (evil-define-key 'normal 'global (kbd ",") 'evil-execute-in-god-state))
