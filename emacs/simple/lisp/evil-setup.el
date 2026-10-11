;;; evil-setup.el --- vim editing, nothing else -*- lexical-binding: t; -*-

;; Comment out the (load "evil-setup") line in init.el to disable this.
;; Named evil-setup.el, not evil.el, so it doesn't shadow the package's own
;; evil.el on `load-path'.

;; Scope: modal editing in buffers where you edit text. Everything else --
;; dired, magit, help, the minibuffer, your C-c bindings -- stays vanilla.
;; No evil-collection, no leader key.

(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)              ; keep TAB as TAB in the terminal
  (setq evil-undo-system 'undo-tree)         ; undo-tree itself lives in core.el
  (setq evil-disable-insert-state-bindings t) ; insert state = plain Emacs
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  :config
  ;; Emacs state everywhere by default; normal state only in editing buffers.
  ;; Evil walks the major-mode parent chain, so these three cover prog, text
  ;; and conf derivatives without listing each mode.
  (setq evil-default-state 'emacs)
  (dolist (mode '(prog-mode text-mode conf-mode))
    (evil-set-initial-state mode 'normal))

  ;; C-g quits to normal state, same reflex as ESC.
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  ;; Move by visual line when text is wrapped.
  (evil-define-key 'normal 'global (kbd "j") 'evil-next-visual-line)
  (evil-define-key 'normal 'global (kbd "k") 'evil-previous-visual-line)

  ;; Half-page scroll. `evil-want-C-u-scroll' above only takes effect when evil
  ;; loads, so bind both explicitly -- that way a re-load of this file during a
  ;; running session fixes C-u too, instead of silently doing nothing.
  ;; C-u stays `universal-argument' in insert and emacs state; in normal state
  ;; evil's own count prefix (10j, d3w) covers what C-u would have done.
  (evil-define-key '(normal visual motion) 'global (kbd "C-u") 'evil-scroll-up)
  (evil-define-key '(normal visual motion) 'global (kbd "C-d") 'evil-scroll-down)

  ;; SPC is C-c. Not a separate leader scheme -- the same `mode-specific-map',
  ;; so every C-c binding in keybindings.el is on SPC for free and which-key
  ;; shows one menu for both. Caveat: major modes put their own keys (org's
  ;; C-c C-c) in the major-mode map, not here, so those stay C-c only.
  (evil-define-key '(normal visual motion) 'global (kbd "SPC") mode-specific-map)

  (evil-mode 1))

;; gcc comments a line, gc takes a motion or a visual selection.
(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode 1))

;; ysiw" adds quotes around a word, cs"' changes them, ds" deletes them.
;; In visual state, S" surrounds the selection.
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))
