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

(global-set-key (kbd "C-v") 'ssm/scroll-up-and-recenter)
(global-set-key (kbd "M-v") 'ssm/scroll-down-and-recenter)

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
