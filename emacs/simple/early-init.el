;;; early-init.el --- pre-GUI frame setup -*- lexical-binding: t; -*-

;; Loaded before the GUI is initialized and before the first frame exists.
;; Chrome is removed via `default-frame-alist' rather than the corresponding
;; modes: toggling those modes manipulates frame parameters, which queues a
;; frame resize the user can see. The mode variables are set by hand so
;; interactive toggling later stays in sync with reality.

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)

(setq menu-bar-mode nil)
(setq tool-bar-mode nil)
(setq scroll-bar-mode nil)

;; The font belongs here too: setting it post-init resizes the frame once the
;; default face changes its character dimensions.
(push '(font . "JetBrains Mono-12") default-frame-alist)

;; Emacs resizes the frame whenever a font or chrome change alters its pixel
;; size. Nothing here needs that, and it costs a visible reflow at startup.
(setq frame-inhibit-implied-resize t)

(setq inhibit-startup-message t)

;; package.el is initialized explicitly from init.el.
(setq package-enable-at-startup nil)
