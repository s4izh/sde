{ config, pkgs, lib, ... }:
let
  dotfiles = /home/sergio/.dotfiles;
in
{
  xdg = {
    enable = true;
#userDirs = {
#  enable = true;
#  createDirectories = true;
#};
  };

  xdg.configFile."user-dirs.dirs".source = "${dotfiles}/.config/user-dirs.dirs";
  # xdg.configFile."user-dirs.locale".source = "${dotfiles}/.config/user-dirs.locale";
  xdg.configFile."mimeapps.list".source = "${dotfiles}/.config/mimeapps.list";

# programs.git = {
#   enable = true;
#   userName = "s4izh";
#   userEmail = "sergiosanz234@gmail.com";
#   extraConfig = {
#     init = {
#       defaultBranch = "main";
#     };
#   };
# };

  xdg.configFile."git/config".source = "${dotfiles}/.config/git/config";

  home.sessionVariables = {
    HISTFILE = "{xdg.dataHome}/bash/bash_history";
  };

# xdg.configFile.<name>.recursive

  # home.file.".ssh/config".source = ./.ssh/config;

  programs.ssh = {
    enable = true;
    matchBlocks = {
      "github.com" = {
        hostname = "github.com";
        user = "git";
        identityFile = "~/.ssh/github";
      };
      "repo.fib.upc.es" = {
        hostname = "repo.fib.upc.es";
        user = "git";
        identityFile = "~/.ssh/repofib";
      };
      "zen" = {
        hostname = "192.168.1.137";
        user = "sergio";
        identityFile = "~/.ssh/zen";
      };
      "pti" = {
        hostname = "nattech.fib.upc.edu";
        user = "alumne";
        port = 22040;
      };
      "sistemes" = {
        hostname = "192.168.122.10";
        user = "alumne";
        identityFile = "~/.ssh/sistemes";
        port = 22;
        forwardX11 = true;
        forwardX11Trusted = true;
      };
    };
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
  };

  gtk = {
    enable = true;
    theme = {
      name = "adw-gtk3-dark";
      package = pkgs.adw-gtk3;
      # name = "Qogir";
      # package = pkgs.qogir-theme;
    };
    iconTheme = {
      name = "Qogir-dark";
      # package = pkgs.papirus-icon-theme;
      package = pkgs.qogir-icon-theme;
    };
    cursorTheme = {
      name = "Qogir";
      package = pkgs.qogir-icon-theme;
    };
    font = {
      # name = "Liberation Mono";
      name = "JetBrains Mono";
      #package = pkgs.rubik;
      size = 11;
    };
    gtk3.extraConfig = { gtk-application-prefer-dark-theme = true; };
    gtk4.extraConfig = { gtk-application-prefer-dark-theme = true; };
    gtk3.bookmarks = [
      "file:///home/sergio/uni/3q2"
      "file:///home/sergio/pkm"
    ];
  };

  dconf.settings = {
    "org/gnome/desktop/interface" = {
      color-scheme = "prefer-dark";
    };
    # "org/gnome/desktop/wm/preferences" = {
    #   button-layout = "appmenu";
    # };
  };

  # programs.firefox = {
  #   enable = true;
  #   profiles.default.extraConfig = ''
  #     user_pref("browser.fullscreen.autohide", false);
  #     user_pref("browser.compactmode.show", true);
  #   '';
  # };

  programs.starship = {
    enable = true;
    settings = {
      add_newline = true;
      format = lib.strings.concatStrings [
        "$os"
        "$directory"
        "$container"
        "$git_branch$git_status"
        "$nix_shell"
        "$python"
        "$nodejs"
        "$lua"
        "$rust"
        "$java"
        "$c"
        "$golang"
        "$status"
        "\n$character"
      ];
    };
  };

# programs.direnv.enable = true;
# programs.direnv.nix-direnv.enable = true;

  home.stateVersion = "23.05";
}
