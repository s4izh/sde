{
  config,
  pkgs,
  lib,
  ...
}:
{
  users.users.sergio = {
    isNormalUser = true;
    description = "sergio";
    extraGroups = [
      "networkmanager"
      "wheel"
    ];
    shell = pkgs.bash;
    packages = with pkgs; [ ];
  };

  programs.zsh.enable = true;

  nix = {
    settings = {
      experimental-features = [
        "nix-command"
        "flakes"
      ];
      auto-optimise-store = true;
    };
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 15d";
      persistent = true;
    };
  };

  nixpkgs.flake.setNixPath = true;
  nixpkgs.flake.setFlakeRegistry = true;

  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
    vim
    # neovim
    vimv
    wget
    git
    pciutils # lspci setpci
    usbutils # lsusb
    pstree
    lshw
    file
    fastfetch
    gnumake
    ripgrep
    tmux
    tree
    du-dust
    tldr
  ];

  # Set your time zone.
  time.timeZone = "Europe/Madrid";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "es_ES.UTF-8";
    LC_IDENTIFICATION = "es_ES.UTF-8";
    LC_MEASUREMENT = "es_ES.UTF-8";
    LC_MONETARY = "es_ES.UTF-8";
    LC_NAME = "es_ES.UTF-8";
    LC_NUMERIC = "es_ES.UTF-8";
    LC_PAPER = "es_ES.UTF-8";
    LC_TELEPHONE = "es_ES.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  console.keyMap = "es";
}
