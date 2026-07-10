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
