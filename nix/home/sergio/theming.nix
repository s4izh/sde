{
  config,
  pkgs,
  lib,
  sde,
  inputs,
  ...
}:
let
  cfg = config.sde.theming;
  gtkExtraCss = builtins.readFile "${sde.flakeRoot}/dotfiles/.config/gtk-3.0/gtk.css";

  bibata = {
    name = "Bibata-Modern-Classic";
    package = pkgs.bibata-cursors;
  };

  isDark = cfg.polarity == "dark";
  pick = dark: light: if isDark then dark else light;

  # each theme sets gtk, icons, cursor and qt for the current polarity, and
  # an accent color (RRGGBB) for the programs colored here (fuzzel).
  # qt is one of:
  #   { style = ...; package = ...; platformTheme = ...; }  plain qt style
  #   { kvantum = { name = ...; package = ...; }; }          kvantum svg theme
  #   { platformTheme = "gtk3"; }                            qt copies the gtk theme
  themes = {
    adwaita = {
      accent = pick "78aeed" "3584e4";
      gtk = {
        name = pick "adw-gtk3-dark" "adw-gtk3";
        package = pkgs.adw-gtk3;
      };
      icons = {
        name = "Adwaita";
        package = pkgs.adwaita-icon-theme;
      };
      cursor = {
        name = "Adwaita";
        package = pkgs.adwaita-icon-theme;
      };
      qt = {
        style = pick "adwaita-dark" "adwaita";
        package = [
          pkgs.adwaita-qt
          pkgs.adwaita-qt6
        ];
      };
    };

    # plasma look outside plasma, qt apps take the colors from kdeglobals
    breeze = {
      accent = "3daee9";
      gtk = {
        name = pick "Breeze-Dark" "Breeze";
        package = pkgs.kdePackages.breeze-gtk;
      };
      icons = {
        name = pick "breeze-dark" "breeze";
        package = pkgs.kdePackages.breeze-icons;
      };
      cursor = {
        name = "breeze_cursors";
        package = pkgs.kdePackages.breeze;
      };
      qt = {
        style = "breeze";
        package = pkgs.kdePackages.breeze;
        platformTheme = "kde";
      };
    };

    # no light kvantum theme, light qt follows the gtk theme
    gruvbox = {
      accent = pick "d79921" "b57614";
      gtk = {
        name = pick "Gruvbox-Dark" "Gruvbox-Light";
        package = pkgs.gruvbox-gtk-theme;
      };
      icons = {
        name = pick "Gruvbox-Plus-Dark" "Gruvbox-Plus-Light";
        package = pkgs.gruvbox-plus-icons;
      };
      cursor = {
        name = "Bibata-Modern-Amber";
        package = pkgs.bibata-cursors;
      };
      qt = pick {
        kvantum = {
          name = "Gruvbox-Dark-Brown";
          package = pkgs.gruvbox-kvantum;
        };
      } { platformTheme = "gtk3"; };
    };

    # same, nordic only ships dark kvantum themes
    nordic = {
      accent = pick "88c0d0" "5e81ac";
      gtk = {
        name = pick "Nordic" "Nordic-Polar";
        package = pkgs.nordic;
      };
      icons = {
        name = pick "Nordzy-dark" "Nordzy";
        package = pkgs.nordzy-icon-theme;
      };
      cursor = {
        name = "Nordic-cursors";
        package = pkgs.nordic;
      };
      qt = pick {
        kvantum = {
          name = "Nordic";
          package = pkgs.nordic;
        };
      } { platformTheme = "gtk3"; };
    };

    catppuccin =
      let
        flavor = pick "mocha" "latte";
      in
      {
        accent = pick "cba6f7" "8839ef";
        gtk = {
          name = "catppuccin-${flavor}-mauve-standard";
          package = pkgs.catppuccin-gtk.override {
            variant = flavor;
            accents = [ "mauve" ];
          };
        };
        icons = {
          name = pick "Papirus-Dark" "Papirus-Light";
          package = pkgs.catppuccin-papirus-folders.override {
            inherit flavor;
            accent = "mauve";
          };
        };
        cursor = {
          name = pick "catppuccin-mocha-dark-cursors" "catppuccin-latte-light-cursors";
          package = pick pkgs.catppuccin-cursors.mochaDark pkgs.catppuccin-cursors.latteLight;
        };
        qt.kvantum = {
          name = "catppuccin-${flavor}-mauve";
          package = pkgs.catppuccin-kvantum.override {
            variant = flavor;
            accent = "mauve";
          };
        };
      };

    # no kvantum theme in nixpkgs for these, qt follows the gtk theme
    tokyonight = {
      accent = pick "7aa2f7" "2e7de9";
      gtk = {
        name = pick "Tokyonight-Dark" "Tokyonight-Light";
        package = pkgs.tokyonight-gtk-theme;
      };
      icons = {
        name = pick "Papirus-Dark" "Papirus-Light";
        package = pkgs.papirus-icon-theme;
      };
      cursor = bibata;
      qt.platformTheme = "gtk3";
    };

    colloid = {
      accent = "3c84f7";
      gtk = {
        name = pick "Colloid-Dark" "Colloid-Light";
        package = pkgs.colloid-gtk-theme;
      };
      icons = {
        name = pick "Colloid-Dark" "Colloid-Light";
        package = pkgs.colloid-icon-theme;
      };
      cursor = bibata;
      qt.platformTheme = "gtk3";
    };

    # manjaro xfce look: matcha sea + papirus maia
    manjaro = {
      accent = "2eb398";
      gtk = {
        name = pick "Matcha-dark-sea" "Matcha-light-sea";
        package = pkgs.matcha-gtk-theme;
      };
      icons = {
        name = pick "Papirus-Dark-Maia" "Papirus-Light-Maia";
        package = pkgs.papirus-maia-icon-theme;
      };
      cursor = {
        name = "breeze_cursors";
        package = pkgs.kdePackages.breeze;
      };
      qt.platformTheme = "gtk3";
    };
  };

  # activates the dark/light specialisation of the generation the last
  # switch installed, then pokes the running apps
  themeMode = pkgs.writeShellApplication {
    name = "theme-mode";
    runtimeInputs = with pkgs; [
      coreutils
      gnugrep
      procps
      systemd
      libnotify
    ];
    text = ''
      state="''${XDG_STATE_HOME:-$HOME/.local/state}/sde/theme-mode"
      current=$(cat "''${XDG_DATA_HOME:-$HOME/.local/share}/sde/polarity" 2>/dev/null || echo dark)

      case "''${1:-toggle}" in
        toggle) if [ "$current" = dark ]; then target=light; else target=dark; fi ;;
        dark | light) target=$1 ;;
        # re-apply the saved mode after a login or switch put back the default
        restore)
          target=$(cat "$state" 2>/dev/null || true)
          if [ -z "$target" ] || [ "$target" = "$current" ]; then exit 0; fi
          ;;
        *)
          echo "usage: theme-mode [toggle|dark|light|restore]" >&2
          exit 1
          ;;
      esac

      mkdir -p "$(dirname "$state")"
      echo "$target" > "$state"
      if [ "$target" = "$current" ]; then exit 0; fi

      # the generation of the last switch, the one with the specialisations.
      # after a toggle current-home points to a specialisation, which has none
      base=$(systemctl show -p ExecStart --value ${"home-manager-${config.home.username}.service"} \
        | grep -o '/nix/store/[^ ;]*-home-manager-generation' | head -n 1 || true)
      if [ ! -d "$base/specialisation" ]; then
        base=$(readlink -f "''${XDG_STATE_HOME:-$HOME/.local/state}/home-manager/gcroots/current-home")
      fi
      if [ ! -x "$base/specialisation/$target/activate" ]; then
        notify-send "theme-mode" "no $target specialisation in $base"
        exit 1
      fi

      log="''${XDG_CACHE_HOME:-$HOME/.cache}/sde/theme-mode.log"
      mkdir -p "$(dirname "$log")"
      "$base/specialisation/$target/activate" > "$log" 2>&1

      # kitty re-reads its config, waybar its style. qt apps need a restart
      pkill -USR1 -x kitty || true
      pkill -USR2 -x waybar || true
      notify-send -t 1500 "theme-mode" "$target"
    '';
  };

  # reloads kitty and waybar whenever the polarity file changes, a toggle or
  # a switch. theme-mode signals them too, a second reload is harmless
  polarityWatch = pkgs.writeShellApplication {
    name = "sde-polarity-watch";
    runtimeInputs = with pkgs; [
      coreutils
      inotify-tools
      procps
    ];
    text = ''
      dir="''${XDG_DATA_HOME:-$HOME/.local/share}/sde"
      mkdir -p "$dir"
      last=$(cat "$dir/polarity" 2>/dev/null || true)
      # the directory, the file is a symlink home-manager replaces
      inotifywait -m -q -e create,moved_to,close_write "$dir" | while read -r _; do
        now=$(cat "$dir/polarity" 2>/dev/null || true)
        if [ -n "$now" ] && [ "$now" != "$last" ]; then
          last=$now
          pkill -USR1 -x kitty || true
          pkill -USR2 -x waybar || true
        fi
      done
    '';
  };

  # lock screen background: a soft fractal in the theme accent, dark to
  # accent or accent to light depending on the mode. fixed seed, same image
  # on every build
  lockImage =
    pkgs.runCommand "sde-lock-${cfg.theme}-${cfg.polarity}.png"
      {
        nativeBuildInputs = [ pkgs.imagemagick ];
      }
      ''
        magick -seed 7 -size 1920x1080 plasma:fractal -blur 0x6 \
          -colorspace gray -auto-level -sigmoidal-contrast 4,50% \
          +level-colors '${pick "#0b100f" "#f2f7f6"}','#${theme.accent}' \
          -modulate ${pick "85" "100"} $out
      '';

  useStylix = cfg.theme == "stylix";
  kittyTheme = pick cfg.kitty.dark cfg.kitty.light;
  theme = themes.${cfg.theme} or { };
  kvantum = theme.qt.kvantum or null;
in
{
  imports = [ inputs.stylix.homeModules.stylix ];

  # GTK/Qt theme and cursor for standalone WMs. Full desktop environments
  # (Plasma, GNOME) manage these themselves and break on the read-only files
  # home-manager writes, so the NixOS side turns this off through
  # sde.desktop.fullEnvironment.
  options.sde.theming.enable = lib.mkOption {
    type = lib.types.bool;
    default = true;
    description = "Whether home-manager manages the GTK/Qt theme and cursor.";
  };

  options.sde.theming.theme = lib.mkOption {
    type = lib.types.enum ((lib.attrNames themes) ++ [ "stylix" ]);
    default = "adwaita";
    description = ''
      Theme used for GTK, Qt, icons and cursor. "stylix" generates everything
      (also terminals, waybar, ...) from the base16 scheme in
      sde.theming.stylixScheme.
    '';
  };

  options.sde.theming.stylixScheme = lib.mkOption {
    type = lib.types.str;
    default = "gruvbox-dark-medium";
    example = "catppuccin-mocha";
    description = "File name (without .yaml) from pkgs.base16-schemes, see share/themes.";
  };

  options.sde.theming.stylixSchemeLight = lib.mkOption {
    type = lib.types.str;
    default = "gruvbox-light-medium";
    example = "catppuccin-latte";
    description = "Like stylixScheme, used when polarity is light.";
  };

  options.sde.theming.polarity = lib.mkOption {
    type = lib.types.enum [
      "dark"
      "light"
    ];
    default = "dark";
    description = "Polarity after a switch or login, theme-mode flips it at runtime.";
  };

  options.sde.theming.kitty = {
    dark = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      example = "gruvbox-dark-hard";
      description = ''
        Kitty theme for the dark mode, a file name (without .conf) in
        dotfiles/.config/kitty/themes. null keeps theme.conf (kitty-theme script).
      '';
    };
    light = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = "zenbones-light";
      example = "Modus_Operandi";
      description = "Like kitty.dark, for the light mode.";
    };
  };

  options.sde.theming.lockImage = lib.mkOption {
    type = lib.types.nullOr lib.types.str;
    default = null;
    example = "~/git/wallpapers/wallhaven-q6p667.jpg";
    description = ''
      Image for swaylock, read when locking (it is not copied to the store).
      null uses one generated in the theme accent color.
    '';
  };

  options.sde.theming.toggle = lib.mkOption {
    type = lib.types.bool;
    default = true;
    description = ''
      Build a dark and a light specialisation of the home configuration and
      install theme-mode, which activates one of them without rebuilding.
    '';
  };

  config = lib.mkMerge [
    # kitty config lives in the dotfiles, kitty.conf includes this file after
    # its own theme.conf. it always exists so the include never fails: stylix
    # colors, the sde.theming.kitty theme of the mode or empty to keep theme.conf
    {
      xdg.dataFile."kitty/sde-theme.conf".text =
        if cfg.enable && useStylix then
          with config.lib.stylix.colors.withHashtag;
          ''
            background ${base00}
            foreground ${base05}
            selection_background ${base05}
            selection_foreground ${base00}
            url_color ${base04}
            cursor ${base05}
            cursor_text_color ${base00}
            active_border_color ${base03}
            inactive_border_color ${base01}
            active_tab_background ${base00}
            active_tab_foreground ${base05}
            inactive_tab_background ${base01}
            inactive_tab_foreground ${base04}
            tab_bar_background ${base01}

            color0 ${base00}
            color1 ${base08}
            color2 ${base0B}
            color3 ${base0A}
            color4 ${base0D}
            color5 ${base0E}
            color6 ${base0C}
            color7 ${base05}
            color8 ${base03}
            color9 ${base08}
            color10 ${base0B}
            color11 ${base0A}
            color12 ${base0D}
            color13 ${base0E}
            color14 ${base0C}
            color15 ${base07}
          ''
        else if cfg.enable && kittyTheme != null then
          "include ${config.xdg.configHome}/kitty/themes/${kittyTheme}.conf\n"
        else
          "# kitty keeps its own theme.conf\n";
    }

    (lib.mkIf cfg.enable (
      lib.mkMerge [
        {
          gtk = {
            enable = true;
            font = lib.mkDefault {
              name = "monospace";
              size = 10;
            };
            gtk3.extraConfig = {
              gtk-application-prefer-dark-theme = isDark;
              # gtk-application-prefer-light-theme = true;
              # gtk-dialogs-use-header = false;
              # gtk-decoration-layout= ":";
            };
            gtk3.extraCss = gtkExtraCss;
            gtk4.extraConfig = {
              gtk-application-prefer-dark-theme = isDark;
              # gtk-application-prefer-light-theme = true;
              # gtk-dialogs-use-header = false;
              # gtk-decoration-layout= ":";
            };
            gtk4.extraCss = gtkExtraCss;
            gtk4.theme = lib.mkDefault config.gtk.theme;
            gtk3.bookmarks = [
              "file:///mnt"
              "file:///home/sergio/notes"
              "file:///home/sergio/notes/docs"
              "file:///home/sergio/uni/miri"
              "file:///home/sergio/uni/mei"
              "file:///home/sergio/uni/gei"
              "file:///home/sergio/personal/tfm"
            ];
            # plasma (kde-gtk-config) writes these files on login, take them back
            # when coming from a KDE session
            gtk2.force = true;
          };

          xdg.configFile."gtk-3.0/settings.ini".force = true;
          xdg.configFile."gtk-4.0/settings.ini".force = true;

          # firefox, libadwaita and the portal read this one. gtk.* already
          # puts gtk-theme/icon-theme/cursor-theme here, so running gtk3 apps
          # follow a theme-mode switch too
          dconf.settings."org/gnome/desktop/interface".color-scheme = lib.mkDefault (
            pick "prefer-dark" "prefer-light"
          );

          # colored below, or by stylix's swaylock target
          programs.swaylock.enable = true;

          # theme-mode reads the active polarity from here
          xdg.dataFile."sde/polarity".text = cfg.polarity + "\n";
        }

        (lib.mkIf (!useStylix) {
          gtk = {
            theme = theme.gtk;
            iconTheme = theme.icons;
            cursorTheme = theme.cursor;
          };

          # copiado del Toomoch
          home.pointerCursor = {
            enable = true;
            inherit (theme.cursor) name package;
            size = 24;
            x11 = {
              enable = true;
              defaultCursor = theme.cursor.name;
            };
            gtk.enable = true;
            # also link the theme into ~/.icons, some older apps only look there
            dotIcons.enable = true;
          };

          # swaylock -f from the session menu and SUPER+ALT+l
          programs.swaylock = {
            settings =
              let
                a = theme.accent;
                none = "00000000";
              in
              {
                image = if cfg.lockImage != null then cfg.lockImage else "${lockImage}";
                scaling = "fill";
                font = "monospace";
                font-size = 22;
                indicator-radius = 110;
                indicator-thickness = 8;
                indicator-idle-visible = true;
                show-failed-attempts = true;
                ignore-empty-password = true;

                ring-color = a;
                ring-ver-color = a;
                ring-clear-color = "e5c07b";
                ring-wrong-color = "e06c75";
                ring-caps-lock-color = "e5c07b";
                key-hl-color = pick "ffffff" "1e1e1e";
                bs-hl-color = "e06c75";
                caps-lock-key-hl-color = pick "ffffff" "1e1e1e";
                caps-lock-bs-hl-color = "e06c75";

                inside-color = pick "000000aa" "ffffffaa";
                inside-ver-color = pick "000000aa" "ffffffaa";
                inside-clear-color = pick "000000aa" "ffffffaa";
                inside-wrong-color = pick "000000aa" "ffffffaa";
                inside-caps-lock-color = pick "000000aa" "ffffffaa";

                text-color = pick "ffffff" "1e1e1e";
                text-ver-color = pick "ffffff" "1e1e1e";
                text-clear-color = pick "ffffff" "1e1e1e";
                text-wrong-color = "e06c75";
                text-caps-lock-color = "e5c07b";

                line-color = none;
                line-ver-color = none;
                line-clear-color = none;
                line-wrong-color = none;
                line-caps-lock-color = none;
                separator-color = none;
              };
          };

          # fuzzel.nix has the rest of the fuzzel config
          programs.fuzzel.settings = {
            main = {
              font = "monospace:size=12";
              icon-theme = theme.icons.name;
            };
            colors =
              let
                a = theme.accent;
              in
              pick
                {
                  background = "1e1e1ef2";
                  text = "dcdcdcff";
                  prompt = "${a}ff";
                  input = "ffffffff";
                  match = "${a}ff";
                  selection = "3a3a3aff";
                  selection-text = "ffffffff";
                  selection-match = "${a}ff";
                  border = "${a}ff";
                }
                {
                  background = "f6f6f6f2";
                  text = "2a2a2aff";
                  prompt = "${a}ff";
                  input = "000000ff";
                  match = "${a}ff";
                  selection = "dcdcdcff";
                  selection-text = "000000ff";
                  selection-match = "${a}ff";
                  border = "${a}ff";
                };
          };

          qt = {
            enable = true;
            style =
              if kvantum != null then
                {
                  name = "kvantum";
                  package = [
                    pkgs.libsForQt5.qtstyleplugin-kvantum
                    pkgs.qt6Packages.qtstyleplugin-kvantum
                  ];
                }
              else
                {
                  name = theme.qt.style or null;
                  package = theme.qt.package or null;
                };
            platformTheme.name = theme.qt.platformTheme or null;
          };
        })

        # plasma writes kdeglobals too, same story as the gtk settings.ini
        (lib.mkIf (cfg.theme == "breeze") {
          xdg.configFile."kdeglobals" = {
            source = "${pkgs.kdePackages.breeze}/share/color-schemes/Breeze${pick "Dark" "Light"}.colors";
            force = true;
          };
        })

        (lib.mkIf (!useStylix && kvantum != null) {
          xdg.configFile."Kvantum/kvantum.kvconfig".text = ''
            [General]
            theme=${kvantum.name}
          '';
          xdg.configFile."Kvantum/${kvantum.name}".source =
            "${kvantum.package}/share/Kvantum/${kvantum.name}";
        })

        (lib.mkIf cfg.toggle {
          specialisation = {
            dark.configuration.sde.theming.polarity = lib.mkForce "dark";
            light.configuration.sde.theming.polarity = lib.mkForce "light";
          };

          home.packages = [ themeMode ];

          systemd.user.services.sde-polarity-watch = {
            Unit = {
              Description = "Reload kitty and waybar on light/dark changes";
              PartOf = [ "graphical-session.target" ];
              After = [ "graphical-session.target" ];
            };
            Service = {
              ExecStart = lib.getExe polarityWatch;
              Restart = "on-failure";
            };
            Install.WantedBy = [ "graphical-session.target" ];
          };
        })

        (lib.mkIf useStylix {
          stylix = {
            enable = true;
            base16Scheme = "${pkgs.base16-schemes}/share/themes/${pick cfg.stylixScheme cfg.stylixSchemeLight}.yaml";
            inherit (cfg) polarity;
            cursor = bibata // {
              size = 24;
            };
            icons = {
              enable = true;
              package = pkgs.papirus-icon-theme;
              dark = "Papirus-Dark";
              light = "Papirus-Light";
            };
            fonts.monospace = {
              name = "Iosevka";
              package = pkgs.iosevka;
            };
            # emacs has its own theme in the emacs config
            # targets.emacs.enable = false;
          };
        })
      ]
    ))
  ];
}
