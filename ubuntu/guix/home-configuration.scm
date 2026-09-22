;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(use-modules (gnu home)
             (gnu packages)
             (gnu services)
             (guix gexp)
             (gnu home services shells))

(home-environment
  ;; Below is the list of packages that will show up in your
  ;; Home profile, under ~/.guix-home/profile.
  (packages (specifications->packages (list "git"
                                            "universal-ctags"
                                            "tmux"
                                            "nss-certs"
                                            "curl"
                                            "emacs"
                                            "emacs-mmm-mode"
                                            "glibc-locales"
                                            "alacritty"
                                            "ugrep"
                                            "clang")))

  ;; Below is the list of Home services.  To search for available
  ;; services, run 'guix home search KEYWORD' in a terminal.
  (services
   (list (service home-bash-service-type
                  (home-bash-configuration
                   (aliases '(("YT" . "youtube-viewer")
                              ; ("alert" . "notify-send --urgency=low -i \"$([ $? = 0 ] && echo terminal || echo error)\" \"$(history|tail -n1|sed -e '\\''s/^\\s*[0-9]\\+\\s*//;s/[;&|]\\s*alert$//'\\'')\"")
                              ("arch-fix-keys" . "sudo pacman-key --init && sudo pacman-key --populate archlinux && sudo pacman-key --refresh-keys")
                              ("ccat" . "highlight --out-format=ansi")
                              ("cfd" . "cd ~/.local/src/dwm && nvim config.h")
                              ("cfdl" . "cd ~/.local/src/dwl && nvim config.h")
                              ("cff" . "nvim ~/.config/foot/foot.ini")
                              ("cfh" . "cd ~/.config/hypr && nvim hyprland.conf")
                              ("cfi" . "cd ~/.config/i3 && nvim config")
                              ("cft" . "nvim ~/.config/tmux/tmux.conf")
                              ; ("cfv" . "cd ~/.config/nvim && nvim $(fzf)")
                              ("cfz" . "cd ~/.config/zsh && nvim .zshrc")
                              ("cl" . "clear")
                              ("cp" . "cp -iv")
                              ("df" . "df -h")
                              ("diff" . "diff --color=auto")
                              ("e" . "nvim")
                              ("egrep" . "egrep --color=auto")
                              ("em" . "emacsclient -nw")
                              ("ffmpeg" . "ffmpeg -hide_banner")
                              ("fgrep" . "fgrep --color=auto")
                              ("free" . "free -m")
                              ("g" . "git")
                              ("ga" . "git add")
                              ("gc" . "git commit -m")
                              ("gco" . "git checkout")
                              ("gm" . "git checkout main")
                              ("gpg-check" . "gpg2 --keyserver-options auto-key-retrieve --verify")
                              ("gpg-retrieve" . "gpg2 --keyserver-options auto-key-retrieve --receive-keys")
                              ("grep" . "grep --color=auto")
                              ("gsr" . "git submodule update --recursive --remote")
                              ("gst" . "git status")
                              ("gsu" . "git submodule update --init --recursive")
                              ("index" . "nvim index*")
                              ("ip" . "ip -color=auto")
                              ("ka" . "killall")
                              ("l" . "ls -CF")
                              ("la" . "ls -A")
                              ("ll" . "ls -alF")
                              ("ls" . "ls --color=auto --group-directories-first")
                              ("mirror" . "sudo reflector -f 30 -l 30 --number 10 --verbose --save /etc/pacman.d/mirrorlist")
                              ("mirrora" . "sudo reflector --latest 50 --number 20 --sort age --save /etc/pacman.d/mirrorlist")
                              ("mirrord" . "sudo reflector --latest 50 --number 20 --sort delay --save /etc/pacman.d/mirrorlist")
                              ("mirrors" . "sudo reflector --latest 50 --number 20 --sort score --save /etc/pacman.d/mirrorlist")
                              ("mkd" . "mkdir -pv")
                              ("mv" . "mv -iv")
                              ("pscpu" . "ps auxf | sort -nr -k 3 | head -5")
                              ("psmem" . "ps auxf | sort -nr -k 4 | head -5")
                              ("qtilecf" . "cd ~/.config/qtile && nvim .")
                              ("rm" . "rm -vI")
                              ("sdn" . "shutdown -h now")
                              ("ssh" . "TERM=xterm-256color ssh")
                              ("systemctl-list" . "systemctl list-unit-files --state=enabled")
                              ("sz" . "source ~/.bashrc")
                              ("tarz" . "tar -xzvf")
                              ("tkS" . "tmux kill-server")
                              ("tks" . "tmux kill-session")
                              ("tn" . "tmux-notes")
                              ("tp" . "tmux-picker")
                              ("trem" . "transmission-remote")
                              ("ts" . "tmux-sessionizer")
                              ("v" . "nvim")
                              ("vim" . "nvim")
                              ("yt" . "yt-dlp --embed-metadata -i")
                              ("yta" . "yt -x -f bestaudio/best")
                              ("zsh-update-plugins" . "find /plugins -type d -exec test -e '\\''{}/.git'\\'' '\\'';'\\'' -print0 | xargs -I {} -0 git -C {} pull -q")))
                   (bashrc (list (local-file ".bashrc" "bashrc")))
                   (bash-logout (list (local-file ".bash_logout"
                                                  "bash_logout"))))))))
