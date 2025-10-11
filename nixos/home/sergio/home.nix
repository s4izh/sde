{
  config,
  pkgs,
  lib,
  sde,
  ...
}:
let
  ln = config.lib.file.mkOutOfStoreSymlink;
  lnDir = config.lib.file.mkOutOfStoreSymlink;
  gtkExtraCss = builtins.readFile "${sde.flakeRoot}/dotfiles/.config/gtk-3.0/gtk.css";
in
{
  imports = [
    ./waybar.nix
    ./river.nix
    ./notes.nix
  ];

  # home.backupFileExtension = "backup";

  home.packages = with pkgs; [
    tmux
    fzf
    shellcheck
    shfmt
    gnumake
    sxhkd
    dunst
    libnotify
    direnv
    git
    delta
  ];

  xdg = {
    enable = true;
    # configFile."mimeapps.list".source = ln "${dotfiles}/.config/mimeapps.list";
    userDirs = {
      enable = true;
      createDirectories = false;
      desktop = "${config.home.homeDirectory}/dt";
      download = "${config.home.homeDirectory}/dl";
      documents = "${config.home.homeDirectory}/docs";
      pictures = "${config.home.homeDirectory}/pix";
      templates = "${config.home.homeDirectory}/templates";
      videos = "${config.home.homeDirectory}/media/vids";
      music = "${config.home.homeDirectory}/media/music";
      publicShare = "${config.home.homeDirectory}/media/pub";
    };
  };

  # home.file.".config/tmux".source = ln $"

  # home.file.".editorconfig".source = ln "${dotfiles}/.editorconfig";

  # xdg.mimeApps.defaultAplications = {
  #   "text/plain" = [ "nvim.desktop" ];
  #   # "aplication/pdf" = [ "zathura.desktop" ];
  #   "image/*" = [ "sxiv.desktop" ];
  #   "video/*" = [ "mpv.desktop" ];
  # };

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

  # home.sessionVariables = {
  #   HISTFILE = "{xdg.dataHome}/bash/bash_history";
  # };

  # home.sessionVariables = {
  #   EDITOR = "nvim";
  #   TERMINAL = "alacritty";
  #   BROWSER = "firefox";
  #   READER = "zathura";
  #   SUDO_ASKPASS = "dmenupass";
  #   MANPAGER = "nvim +Man!";
  # };

  # home.shellAliases = {
  #   ts = "${scripts}/tmux/tmux-sessionizer";
  # };

  # programs.zsh = {
  #   enable = true;
  #   dotDir = ".config/zsh";
  #   enableCompletion = true;
  #   enableSyntaxHighlighting = true;
  #   # defaultKeymap = "vicmd";
  #   history.path = "$ZDOTDIR/zsh/zsh_history";
  # };

  # xdg.configFile."zsh/alias".source = "${dotfiles}/.config/zsh/alias";
  # xdg.configFile."zsh/functions".source = "${dotfiles}/.config/zsh/functions";

  # services.dunst = {
  #   enable = true;
  #   dunst.configFile = "${dotfiles}/.config/dunst/dunstrc";
  # };

  # xdg.configFile.<name>.recursive

  # home.file.".ssh/config".source = ./.ssh/config;

  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;
    matchBlocks = {
      "*" = {
        setEnv = {
          TERM = "xterm-256color";
        };
        extraOptions = {
          AddKeysToAgent = "yes";
        };
      };
      # "sistemes" = {
      #   hostname = "192.168.122.10";
      #   user = "alumne";
      #   identityFile = "~/.ssh/sistemes";
      #   port = 22;
      #   forwardX11 = true;
      #   forwardX11Trusted = true;
      # };
    };
    includes = [
      "config.d/*"
    ];
  };

  # programs.emacs = {
  #   enable = true;
  #   package = pkgs.emacs30-gtk3;
  # };

  gtk = {
    enable = true;
    theme = {
      name = "adw-gtk3-dark";
      package = pkgs.adw-gtk3;
    };
    iconTheme = {
      # name = "Papirus-dark";
      name = "Adwaita";
      # package = pkgs.papirus-icon-theme;
      package = pkgs.adwaita-icon-theme;
    };
    cursorTheme = {
      name = "Adwaita";
      package = pkgs.adwaita-icon-theme;
    };
    font = {
      name = "monospace";
      size = 10;
    };
    gtk3.extraConfig = {
      gtk-application-prefer-dark-theme = true;
      # gtk-application-prefer-light-theme = true;
      # gtk-dialogs-use-header = false;
      # gtk-decoration-layout= ":";
    };
    gtk3.extraCss = gtkExtraCss;
    gtk4.extraConfig = {
      gtk-application-prefer-dark-theme = true;
      # gtk-application-prefer-light-theme = true;
      # gtk-dialogs-use-header = false;
      # gtk-decoration-layout= ":";
    };
    gtk4.extraCss = gtkExtraCss;
    gtk3.bookmarks = [
      "file:///mnt"
      "file:///home/sergio/notes"
      "file:///home/sergio/uni/mei"
      "file:///home/sergio/uni/gei"
    ];
  };

  # copiado del Toomoch
  home.pointerCursor = {
    name = "Adwaita";
    package = pkgs.adwaita-icon-theme;
    size = 24;
    x11 = {
      enable = true;
      defaultCursor = "Adwaita";
    };
    gtk.enable = true;
  };

  dconf.settings = {
    "org/gnome/desktop/interface" = {
      color-scheme = "prefer-dark";
      # color-scheme = "prefer-light";
    };
    # "org/gnome/desktop/wm/preferences" = {
    #   button-layout = "appmenu";
    # };
  };

  programs.mpv = {
    enable = true;
    config = {
      hwdec = "auto";
    };
  };

  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;

  home.stateVersion = "23.11";
}
