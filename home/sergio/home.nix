{ config, pkgs, lib, ... }:
{
  xdg = {
    enable = true;
    #userDirs = {
    #  enable = true;
    #  createDirectories = true;
    #};
  };

  programs.git = {
    enable = true;
    userName = "s4izh";
    userEmail = "sergiosanz234@gmail.com";
    extraConfig = {
      init = {
        defaultBranch = "main";
      };
    };
  };

  # xdg.configFile."git/config".source = ./.config/git/config;

  # xdg.configFile.<name>.recursive

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
      "vmfib" = {
        hostname = "nattech.fib.upc.edu";
        user = "alumne";
        port = 22044;
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
    };
    iconTheme = {
      name = "Papirus-Dark";
      package = pkgs.papirus-icon-theme;
      #package = pkgs.qogir-icon-theme;
    };
    cursorTheme = {
      name = "Adwaita";
      package = pkgs.gnome.adwaita-icon-theme;
    };
    font = {
      name = "Liberation Mono";
      #package = pkgs.rubik;
      size = 11;
    };
    gtk3.extraConfig = {
      gtk-application-prefer-dark-theme = true;
    };
    gtk4.extraConfig = {
      gtk-application-prefer-dark-theme = true;
    };
  };

  home.stateVersion = "22.11";
}
