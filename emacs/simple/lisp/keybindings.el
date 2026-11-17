;;; keybindings.el --- standalone commands and their bindings -*- lexical-binding: t; -*-

;; Deliberately small: the goal is to learn the stock keys, so this file only
;; binds things Emacs has no key for. The cheat sheet below is what replaced
;; the old C-c f / C-c k / C-c g / C-c p / C-c c bindings.
;;
;; Any prefix followed by C-h lists its keys (C-x p C-h, C-x r C-h, M-s C-h),
;; and which-key pops up the same list after a second.
;;
;; Files and buffers
;;   C-x C-f   find file              C-x C-s   save
;;   C-x b     switch buffer          C-x B     consult-buffer (recent files,
;;   C-x k     kill buffer                      bookmarks, preview)
;;   C-x C-b   buffer list            C-x C-j   dired at this file
;;
;; Projects (C-x p) -- replaces C-c f f
;;   C-c p     mine: pick any folder under `ssm/project-roots'
;;   C-x p p   switch project         C-x p f   find file in project
;;   C-x p g   grep in project        C-x p b   project buffer
;;   C-x p c   compile at the root    C-x p d   dired at the root
;;   C-x p k   kill project buffers   C-x p !   shell command at the root
;;   New projects are remembered the first time you use C-x p in them. To
;;   register a whole tree: M-x project-remember-projects-under.
;;
;; Bookmarks and registers (C-x r) -- replaces the quick-access commands
;;   C-x r m   bookmark this file/dir C-x r b   jump to a bookmark
;;   C-x r l   list bookmarks         (they also show up in C-x B)
;;   C-x r SPC save point in register C-x r j   jump to a register
;;
;; Search
;;   C-s / C-r isearch                M-s .     isearch symbol at point
;;   M-s o     occur                  M-%       query-replace (C-M-% regexp)
;;   M-s l     consult-line           M-s r     consult-ripgrep
;;   M-g i     consult-imenu          in *grep*: e edits the matches in place
;;
;; Moving
;;   C-a / C-e line                   M-f / M-b     word
;;   C-M-f / C-M-b  balanced expr     C-M-a / C-M-e function
;;   M-< / M->  buffer start/end      M-g g         go to line
;;   C-v / M-v  page                  C-l           recenter (cycles)
;;   C-u C-SPC  back to previous mark C-x C-SPC     same, across buffers
;;
;; Editing
;;   C-SPC     start region           C-x h     select all
;;   M-h       mark paragraph         C-M-SPC   mark expression
;;   C-w / M-w kill / copy            C-y, M-y  yank, browse kill ring
;;   C-k       kill to end of line    C-S-<backspace> kill whole line
;;   M-d / M-DEL kill word fwd/back   M-z       zap up to a char
;;   C-/       undo                   C-?       redo
;;   M-;       comment dwim           C-x C-;   comment line
;;   M-^       join with line above   M-q       refill paragraph
;;   C-x SPC   rectangle region       C-x r t   replace rectangle with text
;;   F3 / F4   record / run a keyboard macro
;;
;; Windows
;;   C-x o (M-o) other window         C-x 0 / C-x 1   close this / the others
;;   C-x 2 / C-x 3 split              C-x 4 f / b     open in other window
;;   C-c <left> / <right>  undo / redo the window layout (winner-mode)
;;
;; Code
;;   M-.  definition    M-,  back    M-?  references
;;   M-g n / M-g p  next / previous error (compile, grep, flymake)
;;   Eglot has no keys of its own: M-x eglot-rename, eglot-code-actions.
;;
;; Git and help
;;   C-x g     magit-status           C-x v     built-in vc prefix
;;   C-h k / f / v  describe key / function / variable
;;   C-h m     keys of the current modes
;;   C-h b     every active binding   C-h r     the Emacs manual

(defun ssm/toggle-line-numbers ()
  "Toggle line numbers in the current buffer."
  (interactive)
  (display-line-numbers-mode 'toggle))

(global-set-key (kbd "C-c l") 'ssm/toggle-line-numbers)

;; other-window is the most used window command and C-x o is two chords.
;; M-o is unbound by default.
(global-set-key (kbd "M-o") 'other-window)

;; Duplicate the line, or the region when active. Replaces `list-directory',
;; which dired makes redundant.
(global-set-key (kbd "C-x C-d") 'duplicate-dwim)

(defvar ssm/project-roots '("~/personal" "~/devel/lanaccess")
  "Directories whose immediate subdirectories `ssm/find-project' offers.")

(defun ssm/find-project ()
  "Pick a directory under `ssm/project-roots' and find a file in it.
Unlike C-x p p, which only knows projects already visited, this lists
every subdirectory live, so fresh clones and non-git folders show up.
Falls back to Dired when the choice is not a project."
  (interactive)
  (let* ((dirs (seq-mapcat
                (lambda (root)
                  (when (file-directory-p root)
                    (seq-filter #'file-directory-p
                                (directory-files root t directory-files-no-dot-files-regexp))))
                ssm/project-roots))
         (default-directory
          (file-name-as-directory
           (completing-read "Go to project: " (mapcar #'abbreviate-file-name dirs) nil t))))
    (if (project-current)
        (project-find-file)
      (dired default-directory))))

(global-set-key (kbd "C-c p") 'ssm/find-project)

;; Compile. C-x p c (project-compile) prompts for a command and runs it at the
;; project root; M-x compile does the same from the buffer's directory.
;; `recompile' re-runs the last one without asking and has no default key.
(global-set-key (kbd "<f6>") 'recompile)

(defun ssm/set-project-compile-command (command)
  "Write COMMAND as the buffer-local `compile-command' into .dir-locals.el.
The file is created at the project root (or `default-directory' when not in a
project), so `compile'/`recompile' in any buffer under it default to COMMAND.
Merges into an existing .dir-locals.el rather than clobbering it."
  (interactive
   (list (read-shell-command "Project compile command: "
                             (if (boundp 'compile-command) compile-command "make -k"))))
  (let* ((root (or (when-let* ((pr (project-current))) (project-root pr))
                   default-directory))
         (default-directory root))
    ;; add-dir-local-variable is the built-in that safely edits .dir-locals.el;
    ;; it opens the file in a buffer, so save it and report where it landed.
    (save-window-excursion
      (add-dir-local-variable nil 'compile-command command)
      (save-buffer))
    (message "compile-command set in %s.dir-locals.el" root)))

(global-set-key (kbd "C-c c l") 'ssm/set-project-compile-command)
