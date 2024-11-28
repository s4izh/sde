(define-module (sergio home config)
  #:use-module (guix gexp)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services fontutils)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services xdg)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin) ;fastfetch
  #:use-module (sergio utils global)
  #:use-module (sergio packages neovim)
  #:use-module (sergio packages discord)
  #:export (home-config))

(define packages-custom
  (list neovim-latest discord))

(define packages-wayland
  (specifications->packages
    (list "wl-clipboard"
          "wlr-randr"
          "kanshi"
          "wayland-utils"
          "slurp")))

(define packages-general
  (list fastfetch))

(define packages-writing-specifications
  (specifications->packages
    (list "pandoc"
          "texlive"
          "zathura"
          "zathura-ps"
          "zathura-pdf-mupdf")))

(define packages-general-specifications
  (specifications->packages
    (list "alacritty"
          "ripgrep"
          "tree"
          "openfortivpn"
          "remmina"
          "grim"
          "slurp")))

(define packages-home
  (append packages-custom
          packages-wayland
          packages-writing-specifications
          packages-general
          packages-general-specifications))

(define home-config
  (home-environment
    ;; Below is the list of packages that will show up in your
    ;; Home profile, under ~/.guix-home/profile.
    (packages packages-home)

    ;; Below is the list of Home services.  To search for available
    ;; services, run 'guix home search KEYWORD' in a terminal.
    (services
     (list
      ;; (service home-bash-service-type
      ;; (home-bash-configuration
      ;; (aliases '(("grep" . "grep --color=auto")
      ;; ("ip" . "ip -color=auto")
      ;; ("ll" . "ls -l")
      ;; ("ls" . "ls -p --color=auto")
      ;; ("vim" . "nvim")
      ;; ("tKs" . "tmux kill-server")
      ;; ("tks" . "tmux kill-session")
      ;; ("ts" . "tmux-sessionizer")))
      ;; (bashrc (list (local-file ".bashrc" "bashrc")))
      ;; (bash-profile (list (local-file ".bash_profile"
      ;; "bash_profile")))))
      
      ;; (simple-service 'profile-env-vars-service
      ;; home-environment-variables-service-type
      ;; '( ;; Sort hidden (dot) files first in `ls` listings
      ;; ("LC_COLLATE" . "C")
      
      ;; Emacs is our editor
      ;; ("VISUAL" . "nvim")
      ;; ("EDITOR" . "nvim")
      
      ;; Add some things to $PATH (maybe integrate into other services?)
      ;; ("PATH" . "$HOME/.local/scripts/tmux:$HOME/.local/scripts:$PATH")
      ;; ("TERM" . "xterm-256color")
      
      ;; Make sure Flatpak apps are visible
      ;; ("XDG_DATA_DIRS" . "$XDG_DATA_DIRS:$HOME/.local/share/flatpak/exports/share")
      
      ;; Make sure JAVA_HOME is set
      ;; TODO:  Move this to a different service
      ;; ("JAVA_HOME" . "$(dirname $(dirname $(readlink $(which java))))")
      
      ;; Set Wayland-specific environment variables (taken from RDE)
      ;; ("XDG_CURRENT_DESKTOP" . "sway")
      ;; ("XDG_SESSION_TYPE" . "wayland")
      ;; ("RTC_USE_PIPEWIRE" . "true")
      ;; ("SDL_VIDEODRIVER" . "wayland")
      ;; ("MOZ_ENABLE_WAYLAND" . "1")
      ;; ("CLUTTER_BACKEND" . "wayland")
      ;; ("ELM_ENGINE" . "wayland_egl")
      ;; ("ECORE_EVAS_ENGINE" . "wayland-egl")
      ;; ("QT_QPA_PLATFORM" . "wayland-egl")
      ;; ))
      
      (service home-dbus-service-type)

      ;; (remove home-shell-service-type %base-services)
      
      (simple-service 'discord-config-files-service
                      home-xdg-configuration-files-service-type
                      (list `("discord/settings.json" ,(local-file (dotfile
                                                                    ".config/discord/settings.json")))))

      (service home-xdg-user-directories-service-type
               (home-xdg-user-directories-configuration
                 (desktop "$HOME/dt")
                 (download "$HOME/dl")
                 (documents "$HOME/docs")
                 (videos "$HOME/videos")
                 (pictures "$HOME/pics")
                 (templates "$HOME/templates")
                 (publicshare "$HOME/public")
                 (music "$HOME/music")))

      ;; (service home-files-service-type
      ;; `(
      ;; (".config/tmux/tmux.conf" ,(local-file (dotfile ".config/tmux/tmux.conf")))
      ;; (".inputrc" ,(local-file (dotfile ".inputrc") "inputrc"))
      ;; (".guile" ,%default-dotguile)
      ;; (".Xdefaults" ,%default-xdefaults)
      ;; ; (".config/nvim" ,(local-file "../../nvim")))
      ;; ; (".config/tmux/tmux.conf" , (local-file "../../../../dotfiles/.config/tmux/tmux.conf"))
      ;; ))
      
      ;; this fucks up my fonts
      ;; (simple-service 'xdg-files
      ;; home-xdg-configuration-files-service-type
      ;; `(("gdb/gdbinit" ,%default-gdbinit)
      ;; ("nano/nanorc" ,%default-nanorc)))
      
      (simple-service 'link-home-files home-files-service-type
                      `((".config/tmux/tmux.conf" ,(local-file (dotfile ".config/tmux/tmux.conf")))
                        (".inputrc" ,(local-file (dotfile ".config/shell/inputrc")))
                        (".editorconfig" ,(local-file (dotfile ".editorconfig") "editorconfig"))
                        (".config/foot/foot.ini" ,(local-file (dotfile ".config/foot/foot.ini")))
                        (".config/nano/nanorc" ,%default-nanorc)
                        (".bashrc" ,(local-file ".bashrc" "bashrc"))
                        (".bash_profile" ,(local-file ".bash_profile" "bash_profile"))
                        (".config/gdb/gdbinit" ,%default-gdbinit)
                        (".config/guix/channels.scm" ,(local-file channels-file-path))
                        (".guile" ,%default-dotguile)
                        (".Xdefaults" ,%default-xdefaults)))

      (simple-service 'fontconfig home-fontconfig-service-type
                      (list '(alias (family "monospace")
                                    (prefer (family "Iosevka")))))))))
