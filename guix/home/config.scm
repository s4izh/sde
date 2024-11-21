;; Este archivo "home-environment" se puede pasar a 'guix home reconfigure'
;; para reproducir el contenido de su perfil.  Esto es "simbólico": sólo
;; especifica nombres de paquete.  Para reproducir el mismo perfil exacto,
;; también necesita capturar los canales que están siendo usados, como son
;; devueltos por "guix describe".  Vea la sección "Replicando Guix" en el
;; manual.

(define-module (home sergio)
  #:use-module (guix gexp)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services fontutils)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu system shadow)
  #:export     (home-config))

(define home-config
  (home-environment
    ;; Below is the list of packages that will show up in your
    ;; Home profile, under ~/.guix-home/profile.
    (packages (specifications->packages (list "alacritty" "neovim")))

    ;; Below is the list of Home services.  To search for available
    ;; services, run 'guix home search KEYWORD' in a terminal.
    (services
      (list (service home-bash-service-type
                     (home-bash-configuration
                       (aliases '(("grep" . "grep --color=auto")
                                  ("ip" . "ip -color=auto")
                                  ("ll" . "ls -l")
                                  ("ls" . "ls -p --color=auto")
                                  ("vim" . "nvim")
                                  ("ts" . "~/.local/scripts/tmux/tmux-sessionizer")))
                       (bashrc (list (local-file ".bashrc" "bashrc")))
                       (bash-profile (list (local-file ".bash_profile"
                                                       "bash_profile")))))

            (simple-service 'link-home-files
                            home-files-service-type
                            `((".config/tmux/tmux.conf" ,(local-file "../../dotfiles/.config/tmux/tmux.conf"))
                              (".inputrc" ,(local-file "../../dotfiles/.inputrc" "inputrc"))
                              ; (".config/nvim" ,(local-file "../../nvim")))
                              ))

            (simple-service 'fontconfig
                            home-fontconfig-service-type
                            (list
                              '(alias
                                 (family "monospace")
                                 (prefer
                                   (family "Iosevka")))))

            ))))

home-config
