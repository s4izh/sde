;;; tools.el --- general editing/VC tools -*- lexical-binding: t; -*-

(use-package magit
  :config
  :bind (("C-x g" . magit-status)))

(use-package which-key
  :init
  (which-key-mode)
  :custom
  (which-key-idle-delay 1.0))

;; helpful: richer *Help* buffers (source, callers, key info) in place of the
;; built-in describe-* commands.
(use-package helpful
  :bind (("C-h f"   . helpful-callable)   ; functions + macros
         ("C-h v"   . helpful-variable)
         ("C-h k"   . helpful-key)
         ("C-h x"   . helpful-command)
         ("C-h F"   . helpful-function)   ; functions only (not macros)
         ("C-c C-d" . helpful-at-point))) ; describe the symbol under point

;; casual: transient (menu) UIs for modes with hard-to-remember keys. Each menu
;; is on C-o inside its mode; discover the rest from there. casual-suite is the
;; umbrella that pulls in casual + casual-avy + casual-symbol-overlay.
(use-package casual-suite
  :bind ((:map calc-mode-map          ("C-o" . casual-calc-tmenu))
         (:map dired-mode-map         ("C-o" . casual-dired-tmenu))
         (:map isearch-mode-map       ("C-o" . casual-isearch-tmenu))
         (:map Info-mode-map          ("C-o" . casual-info-tmenu))
         (:map ibuffer-mode-map       ("C-o" . casual-ibuffer-tmenu))
         (:map bookmark-bmenu-mode-map ("C-o" . casual-bookmarks-tmenu))
         (:map org-agenda-mode-map    ("C-o" . casual-agenda-tmenu))
         (:map reb-mode-map           ("C-o" . casual-re-builder-tmenu))))

(use-package editorconfig
  :ensure nil
  :config
  (editorconfig-mode 1))

;; ghostel: libghostty-powered terminal emulator backed by a native module.
;; Installed out-of-tree in ~/.local/share/ghostel because the native module is
;; a compiled .so (kept, with the source needed to rebuild it, outside the
;; git-tracked config). Built from source with Zig 0.15.2 rather than the
;; auto-downloaded prebuilt binary, which is not linked against the Nix glibc.
(defun ssm/ghostel-new ()
  "Create a fresh Ghostel terminal instead of switching to an existing one."
  (interactive)
  ;; A non-numeric prefix arg makes `ghostel' spawn a new buffer.
  (ghostel '(4)))

(defun ssm/ghostel-window ()
  "Return a live window showing a Ghostel buffer, or nil."
  (seq-find (lambda (win)
              (with-current-buffer (window-buffer win)
                (derived-mode-p 'ghostel-mode)))
            (window-list)))

(defun ssm/ghostel-toggle ()
  "Show, focus, or hide a Ghostel terminal.
Not visible: open it. Visible but not focused: jump to it. Focused:
hide the window, leaving the process running."
  (interactive)
  (let ((win (ssm/ghostel-window)))
    (cond ((null win) (ghostel))
          ((eq win (selected-window)) (delete-window win))
          (t (select-window win)))))

(defun ssm/ghostel-rename (name)
  "Rename the current Ghostel buffer to NAME.
Ghostel stops applying terminal-title renames after a manual rename,
so the name sticks until you change it again."
  (interactive
   (progn
     (unless (derived-mode-p 'ghostel-mode)
       (user-error "Not in a Ghostel buffer"))
     (list (read-string "Rename terminal to: "))))
  (when (string-empty-p (string-trim name))
    (user-error "Terminal name cannot be empty"))
  (rename-buffer (format "*ghostel: %s*" (string-trim name)) t))

(use-package ghostel
  :ensure nil
  :load-path "~/.local/share/ghostel/lisp"
  :commands (ghostel ghostel-project ssm/ghostel-toggle ssm/ghostel-rename)
  ;; C-c t <key>: terminal management (creating/switching from outside a term).
  ;; In-terminal keys are ghostel's own (C-c C-j semi-char, C-c C-t copy, ...).
  :bind (("C-c t t" . ssm/ghostel-toggle)            ; show / focus / hide term
         ("C-c t T" . ghostel)                       ; open/switch default term
         ("C-c t r" . ssm/ghostel-rename)            ; rename current term
         ("C-c t n" . ssm/ghostel-new)               ; new fresh term
         ("C-c t p" . ghostel-project)               ; term at project root
         ("C-c t o" . ghostel-other)                 ; next term, or create one
         ("C-c t l" . ghostel-list-buffers)          ; pick a term to switch to
         ("C-c t L" . ghostel-project-list-buffers)  ; pick a term in project
         ("C-c t ]" . ghostel-next)                  ; cycle to next term
         ("C-c t [" . ghostel-previous))             ; cycle to previous term
  :custom
  ;; The .so is our locally-built one; never try to download or compile on use.
  (ghostel-module-auto-install nil)
  (ghostel-module-directory (expand-file-name "~/.local/share/ghostel/"))
  ;; M-x ghostel-module-compile (to update the module) pulls the pinned Zig via
  ;; nix on demand, so no old zig lands on PATH alongside the system's 0.16.
  (ghostel-module-compile-command
   "nix shell nixpkgs#zig_0_15 --command zig build --prefix %s -Doptimize=ReleaseFast -Dcpu=baseline"))

;; emacs-reader: MuPDF-backed document viewer (replaces doc-view/pdf-tools).
;; Provided by nix (nixos/pkgs/emacs-reader.nix) because it needs a dynamic
;; module linked against MuPDF, hence :ensure nil and no :load-path.
;;
;; Its autoloads file isn't loaded by the nix emacs wrapper, so the
;; auto-mode-alist entries are declared here. Upstream also grabs docx/pptx/
;; xlsx/od*; left out on purpose so editing office files still hits the
;; normal modes.
;;
;; Guarded on the library actually being present: this file is symlinked, so it
;; goes live the moment Emacs restarts, while the package only appears after a
;; nixos-rebuild. Without the guard, that window leaves .pdf mapped to a
;; reader-mode that can't be autoloaded. With it, PDFs just fall back to
;; doc-view until the rebuild lands.
(use-package reader
  :ensure nil
  :if (locate-library "reader")
  :commands (reader-open-doc reader-mode)
  :mode ("\\.\\(pdf\\|epub\\|mobi\\|fb2\\|xps\\|cbz\\)\\'" . reader-mode))
