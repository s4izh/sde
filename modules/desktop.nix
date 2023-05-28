{ config, pkgs, lib, ... }:
{
  environment.systemPackages = with pkgs; [
    firefox
    alacritty
    discord
    dmenu
    w3m
    pavucontrol
    pulsemixer
    zathura
    feh
    fzf
    redshift
    networkmanagerapplet
    gnome.gnome-calculator
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
    exa
    picom
    # compile tools
    nodejs
    libtool
    # c tooling
    gcc
    cmake
    clang
    clang-tools
    # llvmPackages_15.libclang
    # clang-wrapper
    # rust tooling
    cargo
    cargo-watch
    cargo-make
    rust-analyzer
    # rustup esto no hace falta
    rustfmt
    rustc
    # bacon
    diesel-cli
    # python tooling
    python3
    # nix
    rnix-lsp
    # erlang
    erlang
    erlang-ls
    rebar3
    #bash
    #bash-language-server
    nodePackages_latest.bash-language-server
    # linux
    man-pages
    # dev
    starship
    nmap
    jq
    # postman
    gdb
    cloc
    pstree
    # writting
    pandoc
    haskellPackages.pandoc-crossref
    # pandoc-crossref
    # pandoc-citeproc
    texlive.combined.scheme-full
    aspell
    # libreoffice-fresh
    rclone
    # python
    python39Packages.setuptools
    # vimb
    pdftk
    # vpn
    openfortivpn
    groff
    newsboat
    dpkg
    shellcheck
    xdg-ninja
    mandoc
    man-db
  ];

  # programs.starship.enable = true;


  services.xserver = {
    layout = "es";
    xkbVariant = "";
    #xkbOptions = "caps:escape";
    xkbOptions = "ctrl:nocaps";
    libinput = {
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
  };

  programs.npm.enable = true;
  programs.thunar.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  sound.enable = true;
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

  fonts.fonts = with pkgs; [
    iosevka
  ];

  fonts.fontconfig = {
    defaultFonts = {
      #sansSerif = [ "Liberation Mono" ];
      #serif = [ "Liberation Mono" ];
      monospace = [ "Liberation Mono" ];
    };
  };

  documentation.man.generateCaches = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11"; # Did you read the comment?
}
