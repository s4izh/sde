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
in
{
  imports = [
    ./waybar.nix
    ./river.nix
    ./notes.nix
    ./screen-control.nix
    ./beets.nix
    ./theming.nix
    ./fuzzel.nix
  ];

  # home.backupFileExtension = "backup";

  home.packages = with pkgs; [
    tmux
    fzf
    shellcheck
    shfmt
    gnumake
    sxhkd
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
      # Keep exporting XDG_DESKTOP_DIR, XDG_DOWNLOAD_DIR, etc. as session env
      # vars (systemd user env + session variables) so any process can read
      # them from $ENV. The home-manager default flipped true -> false; we
      # pin true to preserve the old behavior (false would only write
      # ~/.config/user-dirs.dirs without exporting the paths).
      setSessionVariables = true;
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
    settings = {
      "*" = {
        SetEnv = {
          TERM = "xterm-256color";
        };
        AddKeysToAgent = "yes";
      };
      # "sistemes" = {
      #   HostName = "192.168.122.10";
      #   User = "alumne";
      #   IdentityFile = "~/.ssh/sistemes";
      #   Port = 22;
      #   ForwardX11 = true;
      #   ForwardX11Trusted = true;
      # };
    };
    includes = [
      "config.d/*"
    ];
  };

  programs.emacs = {
    enable = true;
    # pgtk = native Wayland (no XWayland). Good on wlroots (mango): the
    # compositor draws server-side decorations. On GNOME/Mutter there are NO
    # decorations (Mutter won't draw SSD, Emacs can't draw its own CSD), so on
    # a GNOME host switch to emacs30-gtk3 to get the normal title bar via X11.
    package = pkgs.emacs-pgtk;

    extraPackages = epkgs: [
      epkgs.treesit-grammars.with-all-grammars
    ];

    # (epkgs.treesit-grammars.with-plugins (p: [
    #  p.tree-sitter-nix
    #  p.tree-sitter-python
    #  p.tree-sitter-rust
    #  p.tree-sitter-typescript
    # ]))
  };

  dconf.settings = {
    # color-scheme is set by theming.nix (sde.theming.polarity)
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
