{
  config,
  pkgs,
  lib,
  inputs,
  ...
}:
let
  hostfile = "/home/sergio/personal/sde/dotfiles/etc/hosts";
in
{
  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
    # (firefox.override { nativeMessagingHosts = [ inputs.pipewire-screenaudio.packages.${system}.default ]; })
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
    baobab # disk usage

    # -- terminal goodies --
    yazi
    ueberzugpp # image preview
  ];

  # programs.starship.enable = true;

  programs.nix-ld.enable = true;

  programs.firefox = {
    enable = true;
    package = (
      pkgs.firefox.override {
        nativeMessagingHosts = [ inputs.pipewire-screenaudio.packages.${pkgs.system}.default ];
      }
    );
    preferences = {
      "browser.fullscreen.autohide" = false;
      "browser.compactmode.show" = true;
    };
  };

  services.guix.enable = true;

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
    # unifont # Unifont
    unifont_upper
    # gnu-freefont
    fantasque-sans-mono # Fantasque Sans Mono
    input-fonts
    # fira-code-nerd-font # FiraCode Nerd Font
    # (nerdfonts.override { fonts = [ "LiterationMono Nerd Font Mono" ]; })
    commit-mono # Commit Mono
    # ubuntu-mono # Ubuntu Mono
    nerdfonts
    # inconsolata-nerd-font
    texlivePackages.inconsolata-nerd-font
    texlivePackages.gnu-freefont
    victor-mono
    liberation_ttf
    # FiraCode Nerd Font
  ];

  nixpkgs.config.input-fonts.acceptLicense = true;

  fonts.fontconfig = {
    defaultFonts = {
      #sansSerif = [ "Liberation Mono" ];
      serif = [ "Source Serif Pro" ];
      # monospace = [ "InputMonoCondensed" ];
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

  networking.extraHosts = builtins.readFile hostfile;

  programs.ssh = {
    startAgent = true;
    enableAskPassword = true;
    askPassword = "${pkgs.gnome.seahorse}/libexec/seahorse/ssh-askpass";
  };

  services.tailscale = {
    enable = true;
  };

  # va muy lento con esto
  # documentation.man.generateCaches = true;
}
