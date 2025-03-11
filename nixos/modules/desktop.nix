{
  config,
  pkgs,
  lib,
  inputs,
  sde,
  ...
}:
let
  hostfile = "${sde.flakeRoot}/dotfiles/etc/hosts";
in
{
  # config = {
  #   test.services.desktop.enable = true;

  environment.systemPackages = with pkgs; [
    alacritty
    kitty
    discord
    discord-canary
    dmenu
    w3m
    pavucontrol
    pulsemixer
    zathura
    sioyek
    feh
    # fzf
    redshift
    networkmanagerapplet
    gnome-calculator
    nautilus
    xclip
    maim
    imagemagick
    sshfs
    unzip
    zip
    cowsay
    mpv
    yt-dlp
    xdotool
    picom
    headsetcontrol
    sxiv
    htop
    resources

    x11vnc # remote desktop

    # -- compile tools --
    nodejs # for copilot
    libtool

    # -- python tooling --
    python3

    # -- dev --
    starship
    nmap
    netscanner
    xdg-ninja

    # -- writting --
    pandoc
    haskellPackages.pandoc-crossref
    groff
    aspell
    obsidian
    marksman

    libreoffice-fresh
    rclone
    # vimb
    pdftk

    # -- notifications --
    dunst
    libnotify

    # other
    newsboat
    cointop
    screenkey
    sdcv # dictionary
    pywal
    baobab # disk usage

    # -- terminal goodies --
    yazi
    ueberzugpp # image preview

    chromium
    qmk

    aria2

    wtype # similar to xdotool for wayland

    tree-sitter
    # tree-sitter-grammars
  ];

  # programs.starship.enable = true;

  programs.nix-ld.enable = true;
  services.envfs.enable = true;

  programs.firefox = {
    enable = true;
    package = pkgs.firefox;
    preferences = {
      "browser.fullscreen.autohide" = false;
      "browser.compactmode.show" = true;
      "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
    };
  };

  services.xserver = {
    xkb.layout = "es";
    xkb.variant = "";
    xkb.options = "caps:escape";
  };

  services.libinput = {
    enable = true;
    mouse = {
      accelProfile = "flat";
      middleEmulation = false;
    };
    touchpad = {
      accelProfile = "flat";
      naturalScrolling = true;
    };
  };

  # fix java apps
  environment.sessionVariables._JAVA_AWT_WM_NONREPARENTING = "1";

  programs.npm.enable = true;

  programs.thunar.enable = true;
  programs.thunar.plugins = with pkgs.xfce; [
    thunar-media-tags-plugin
    thunar-archive-plugin
    thunar-volman
  ];

  # automounting
  services.gvfs.enable = true;
  services.dbus.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  # sound.enable = true;

  # services.pulseaudio.enable = false;
  services.pulseaudio.enable = false;

  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    wireplumber.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # check the names with fc-list

  nixpkgs.config.input-fonts.acceptLicense = true;

  fonts = {
    packages = with pkgs; [
      iosevka # Iosevka
      jetbrains-mono # JetBrains Mono
      source-code-pro # Source Code Pro
      source-serif-pro # Source Serif Pro
      # unifont # Unifont
      unifont_upper
      # gnu-freefont
      fantasque-sans-mono # Fantasque Sans Mono
      input-fonts
      # fira-code-nerd-font # FiraCode Nerd Font
      # (nerdfonts.override { fonts = [ "LiterationMono Nerd Font Mono" ]; })
      commit-mono # Commit Mono
      # ubuntu-mono # Ubuntu Mono
      # nerdfonts
      texlivePackages.inconsolata-nerd-font
      texlivePackages.gnu-freefont
      victor-mono
      liberation_ttf
      ibm-plex
      font-awesome
      noto-fonts-emoji
      rubik
      # FiraCode Nerd Font
    ];
    fontconfig = {
      defaultFonts = {
        #sansSerif = [ "Liberation Mono" ];
        serif = [ "Source Serif Pro" ];
        # monospace = [ "InputMonoCondensed" ];
        # monospace = [ "IBM Plex Mono" ];
        # monospace = [ "FiraCode Nerd Font" ];
        # monospace = [ "LiterationMono Nerd Font" ];
        # monospace = [ "JetBrains Mono" ];
        # monospace = [ "UbuntuMono Nerd Font" ];
        monospace = [ "Iosevka" ];
        # monospace = [ "Liberation Mono" ];
        # monospace = [ "Inconsolata Nerd Font" ];
        # monospace = [ "FreeMono" ];
        # monospace = [ "Commit Mono" ];
        # monospace = [ "Victor Mono" ];
      };
    };
  };

  networking.extraHosts = builtins.readFile hostfile;

  programs.ssh = {
    startAgent = true;
    enableAskPassword = true;
    askPassword = "${pkgs.seahorse}/libexec/seahorse/ssh-askpass";
  };

  xdg.portal.enable = true;
  xdg.portal.xdgOpenUsePortal = true;

  qt = {
    enable = true;
    style = "adwaita-dark";
  };

  environment.sessionVariables = {
    QT_STYLE_OVERRIDE = "adwaita-dark";
  };

  # services.greetd = {
  #   enable = true;
  #   settings = {
  #     default_session = {
  #       command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --remember --cmd launch-desktop-session.sh";
  #       user = "sergio";
  #     };
  #   };
  # };

  # systemd.services.greetd.serviceConfig = {
  #   Type = "idle";
  #   StandardInput = "tty";
  #   StandardOutput = "tty";
  #   StandardError = "journal"; # Without this errors will spam on screen
  #   # Without these bootlogs will spam on screen
  #   TTYReset = true;
  #   TTYVHangup = true;
  #   TTYVTDisallocate = true;
  # };

  # va muy lento con esto
  # documentation.man.generateCaches = true;
}
