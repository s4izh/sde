;;; keybindings.el --- standalone commands and their bindings -*- lexical-binding: t; -*-

(defun ssm/toggle-line-numbers ()
  "Toggle line numbers in the current buffer."
  (interactive)
  (display-line-numbers-mode 'toggle))

(global-set-key (kbd "C-c l") 'ssm/toggle-line-numbers)

(defun ssm/fzf-proyectos ()
  "Search directories under ~/personal and ~/devel and jump to their files without tabs."
  (interactive)
  (let* ((cmd "find ~/personal ~/devel/lanaccess -maxdepth 1 -mindepth 1 -type d 2>/dev/null")
         (dirs (split-string (shell-command-to-string cmd) "\n" t))
         (choice (completing-read "Go to project: " dirs)))
    (when choice
      (let ((default-directory choice))
        ;; if it's a valid project (Git, Makefile, etc.), list its files
        ;; otherwise just open the directory in Dired
        (if (project-current)
            (project-find-file)
          (find-file choice))))))

(global-set-key (kbd "C-c p") 'ssm/fzf-proyectos)

(defun ssm/scroll-up-and-recenter ()
  "Page down and recenter point, like vim's C-d + zz."
  (interactive)
  (scroll-up-command)
  (recenter))

(defun ssm/scroll-down-and-recenter ()
  "Page up and recenter point, like vim's C-u + zz."
  (interactive)
  (scroll-down-command)
  (recenter))

;; (global-set-key (kbd "C-v") 'ssm/scroll-up-and-recenter)
;; (global-set-key (kbd "M-v") 'ssm/scroll-down-and-recenter)

(defvar ssm/quick-access-locations
  '(("emacs" . "~/.emacs.d/lisp")
    ("nixos" . "~/personal/sde/nixos")
    ("notes" . "~/notes"))
    ;; ("nvim" . "~/.config/nvim")
    ;; ("hypr" . "~/.config/hypr")
    ;; ("kitty" . "~/.config/kitty")
    ;; ("mango" . "~/.config/mango")
    ;; ("zsh" . "~/.config/zsh"))
  "Alist of name to directory, for `ssm/quick-access-dired' and
`ssm/quick-access-find-file'. Add more entries as needed, no new
keybinding required.")

(defun ssm/quick-access-dired ()
  "Open Dired at one of `ssm/quick-access-locations'."
  (interactive)
  (let* ((name (completing-read "Dired: " (mapcar #'car ssm/quick-access-locations)))
         (dir (cdr (assoc name ssm/quick-access-locations))))
    (dired (expand-file-name dir))))

(defun ssm/quick-access-find-file ()
  "Fuzzy find a file within one of `ssm/quick-access-locations'."
  (interactive)
  (let* ((name (completing-read "Find file in: " (mapcar #'car ssm/quick-access-locations)))
         (dir (expand-file-name (cdr (assoc name ssm/quick-access-locations))))
         (cmd (format "find %s -type f -not -path '*/.git/*'" (shell-quote-argument dir)))
         (files (split-string (shell-command-to-string cmd) "\n" t))
         (choice (completing-read (format "%s file: " name) files)))
    (when choice (find-file choice))))

(global-set-key (kbd "C-c c d") 'ssm/quick-access-dired)
(global-set-key (kbd "C-c c f") 'ssm/quick-access-find-file)

;; Compile. `compile' prompts for a shell command (default `make -k') and runs
;; it from the current buffer's directory; `recompile' silently re-runs the
;; last one. Project-wide builds stay on the built-in C-x p c (project-compile).
;; f5/f6 mirror the leader keys for one-press access; recompile is the fast one.
(global-set-key (kbd "C-c c c") 'compile)     ; SPC c c
(global-set-key (kbd "C-c c r") 'recompile)   ; SPC c r
(global-set-key (kbd "<f5>")    'compile)
(global-set-key (kbd "<f6>")    'recompile)

(defun ssm/set-project-compile-command (command)
  "Write COMMAND as the buffer-local `compile-command' into .dir-locals.el.
The file is created at the project root (or `default-directory' when not in a
project), so `compile'/`recompile' in any buffer under it default to COMMAND.
Merges into an existing .dir-locals.el rather than clobbering it."
  (interactive
   (list (read-shell-command "Project compile command: "
                             (if (boundp 'compile-command) compile-command "make -k"))))
  (let* ((root (or (when-let ((pr (project-current))) (project-root pr))
                   default-directory))
         (default-directory root))
    ;; add-dir-local-variable is the built-in that safely edits .dir-locals.el;
    ;; it opens the file in a buffer, so save it and report where it landed.
    (save-window-excursion
      (add-dir-local-variable nil 'compile-command command)
      (save-buffer))
    (message "compile-command set in %s.dir-locals.el" root)))

(global-set-key (kbd "C-c c l") 'ssm/set-project-compile-command)  ; SPC c l (dir-Locals)

;; The C-c map doubles as the evil leader (see evil-setup.el), so these are
;; reachable as both C-c <key> and SPC <key>. Single letters on C-c are
;; reserved for the user, so nothing here fights a package.

(global-set-key (kbd "C-c SPC") 'execute-extended-command)  ; SPC SPC -> M-x
(global-set-key (kbd "C-c k")   'kill-current-buffer)
(global-set-key (kbd "C-c g")   'magit-status)

;; C-c f <key>: find things. Plain Emacs commands on the lowercase key --
;; vertico and orderless already make `switch-to-buffer' and `find-file' good.
;; Shift gets the consult variant, so it's a toggle within the same prefix
;; when you want previews and consult's extra sources (recent files, bookmarks).
(global-set-key (kbd "C-c f f") 'project-find-file)
(global-set-key (kbd "C-c f F") 'find-file)
(global-set-key (kbd "C-c f b") 'switch-to-buffer)
(global-set-key (kbd "C-c f B") 'consult-buffer)
(global-set-key (kbd "C-c f l") 'consult-line)     ; find a line in this buffer
(global-set-key (kbd "C-c f g") 'consult-ripgrep)  ; grep across the project
(global-set-key (kbd "C-c f i") 'consult-imenu)    ; find a definition here

;; No C-c w window bindings: C-x o / C-x 1 / C-x 2 / C-x 3 already cover it,
;; and evil keeps vim's C-w on top of that. C-c w is left free.
