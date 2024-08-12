{
  config,
  pkgs,
  lib,
  ...
}: {
  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
    firefox
    alacritty
    discord
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

    x11vnc # remote desktop

    # -- compile tools --
    nodejs # for copilot
    libtool

    # -- python tooling --
    python3

    # -- dev --
    starship
    nmap
    xdg-ninja

    # -- writting --
    pandoc
    haskellPackages.pandoc-crossref
    groff
    aspell
    obsidian

    libreoffice-fresh
    rclone
    # vimb
    pdftk

    # -- vpn --
    openfortivpn

    # -- notifications --
    dunst
    libnotify

    # other
    newsboat
    cointop
    screenkey
    sdcv # dictionary
    pywal
  ];

  # programs.starship.enable = true;

  programs.nix-ld.enable = true;

  programs.firefox = {
    enable = true;
    preferences = {
      "browser.fullscreen.autohide" = false;
      "browser.compactmode.show" = true;
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

  # automounting
  # services.gvfs.enable = true;
  # services.dbus.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  # sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # check the names with fc-list
  fonts.packages = with pkgs; [
    iosevka # Iosevka
    jetbrains-mono # JetBrains Mono
    source-code-pro # Source Code Pro
    source-serif-pro # Source Serif Pro
    unifont # Unifont
    fantasque-sans-mono # Fantasque Sans Mono
    input-fonts
    # fira-code-nerd-font # FiraCode Nerd Font
    # (nerdfonts.override { fonts = [ "LiterationMono Nerd Font Mono" ]; })
    nerdfonts
    # LiterationMono Nerd Font Mono
    # FiraCode Nerd Font
  ];

  nixpkgs.config.input-fonts.acceptLicense = true;

  fonts.fontconfig = {
    defaultFonts = {
      #sansSerif = [ "Liberation Mono" ];
      serif = ["Source Serif Pro"];
      # monospace = ["InputMonoCondensed"];
      # monospace = [ "UbuntuMono Nerd Font" ];
      # monospace = ["Iosevka"];
      # monospace = [ "FiraCode Nerd Font" ];
      # monospace = [ "LiterationMono Nerd Font" ];
      monospace = [ "JetBrains Mono" ];
    };
  };

  networking.extraHosts = builtins.readFile /home/sergio/.dotfiles/etc/hosts;

  # xdg.portal.enable = true;
  # xdg.portal.extraPortals = with pkgs; [ xdg-desktop-portal-gtk ];

  # system.userActivationScripts.alacrittyLink.text = ''
  #   if [[ ! -h "$HOME/.config/alacritty" ]]; then
  #     ln -s "$HOME/.dotfiles/.config/alacritty" "$HOME/.config/alacritty"
  #   fi
  # '';

  # va muy lento con esto
  # documentation.man.generateCaches = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  # system.stateVersion = "23.05"; # Did you read the comment?
}
