;;; appearance.el --- visual chrome, scrolling, theme and font -*- lexical-binding: t; -*-

;; Frame chrome (menu/tool/scroll bars), the default font and
;; `inhibit-startup-message' are handled in early-init.el, before the first
;; frame is created. `tooltip-mode' is not a frame parameter, so it stays here.
(tooltip-mode -1)

(global-display-line-numbers-mode 0) ; show line numbers in the margin
(column-number-mode)                 ; show column number in the mode line

;; native emacs smooth scrolling
(pixel-scroll-precision-mode 1)

;; a non-zero scroll-margin fights pixel-scroll-precision-mode's mouse-wheel
;; scrolling, making point warp mid-scroll -- keeping it at 0 in exchange for
;; giving up look-ahead margin on keyboard scrolling
(setq scroll-margin 0)
(setq auto-window-vscroll nil)

;; scroll 1 line at a time (minimal) once inside the margin, instead of
;; recentering in a jump
(setq scroll-conservatively 101)

(setq-default display-line-numbers-width 3)
(setq-default display-line-numbers-grow-only t)

(use-package spaceway-theme
  :ensure nil
  :load-path "lisp/spaceway/"
  :config ())

(use-package spaceway-light-theme
  :ensure nil
  :load-path "lisp/spaceway/"
  :config ())

(use-package ef-themes
  :defer t)

(use-package doom-themes
  :defer t)

;; light/dark comes from ~/.local/share/sde/polarity, written by the nix
;; theming (theme-mode in nixos/home/sergio/theming.nix). THEME_IS_LIGHT from
;; the kitty theme is the fallback when the file is not there.
(defvar ssm/polarity-file
  (expand-file-name "sde/polarity" (or (getenv "XDG_DATA_HOME") "~/.local/share")))

(defvar ssm/themes '((dark . ef-elea-dark) (light . modus-operandi))
  "Theme loaded for each polarity.")

(defun ssm/read-polarity ()
  "Return `light' or `dark' from `ssm/polarity-file', nil if unreadable.
Home-manager swaps the symlink on every toggle, so it can be missing for
a moment."
  (when (file-readable-p ssm/polarity-file)
    (with-temp-buffer
      (insert-file-contents ssm/polarity-file)
      (pcase (string-trim (buffer-string))
        ("light" 'light)
        ("dark" 'dark)))))

(defvar ssm/polarity
  (or (ssm/read-polarity)
      (if (equal (getenv "THEME_IS_LIGHT") "1") 'light 'dark)))

(setq ssm/current-theme (alist-get ssm/polarity ssm/themes))

(defun ssm/load-theme (theme)
  "Load THEME. Picked by hand, it becomes the theme of the current polarity."
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar #'symbol-name
                                     (custom-available-themes))))))
  (when (called-interactively-p 'any)
    (setf (alist-get ssm/polarity ssm/themes) theme))
  (disable-theme ssm/current-theme)
  (setq ssm/current-theme theme)
  (load-theme theme t))

(ssm/load-theme ssm/current-theme)

(defun ssm/polarity-changed (_event)
  (let ((p (ssm/read-polarity)))
    (when (and p (not (eq p ssm/polarity)))
      (setq ssm/polarity p)
      (ssm/load-theme (alist-get p ssm/themes)))))

;; watch the directory, not the file: the symlink gets replaced, which would
;; drop a watch on the file itself after the first toggle. works the same in
;; the daemon, no need to go through emacsclient
(require 'filenotify)
(let ((dir (file-name-directory ssm/polarity-file)))
  (when (file-directory-p dir)
    (file-notify-add-watch dir '(change) #'ssm/polarity-changed)))
