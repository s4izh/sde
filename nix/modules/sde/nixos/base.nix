{
  config,
  pkgs,
  lib,
  options,
  ...
}:
let
  cfg = config.sde.base;
in
{
  options.sde.base = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Enable base SDE system configuration (user, nix settings, locale, essential tools).";
    };
  };

  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
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
        vimv
        wget
        git
        pciutils
        usbutils
        hwloc
        pstree
        psmisc
        lshw
        file
        fastfetch
        gnumake
        ripgrep
        tmux
        tree
        dust
        tldr
        traceroute
        mtr
        lsof
      ];

      time.timeZone = lib.mkDefault "Europe/Madrid";

      i18n.defaultLocale = lib.mkDefault "en_US.UTF-8";
      i18n.extraLocaleSettings = {
        LC_ADDRESS = lib.mkDefault "es_ES.UTF-8";
        LC_IDENTIFICATION = lib.mkDefault "es_ES.UTF-8";
        LC_MEASUREMENT = lib.mkDefault "es_ES.UTF-8";
        LC_MONETARY = lib.mkDefault "es_ES.UTF-8";
        LC_NAME = lib.mkDefault "es_ES.UTF-8";
        LC_NUMERIC = lib.mkDefault "es_ES.UTF-8";
        LC_PAPER = lib.mkDefault "es_ES.UTF-8";
        LC_TELEPHONE = lib.mkDefault "es_ES.UTF-8";
        LC_TIME = lib.mkDefault "en_US.UTF-8";
      };

      services.xserver.xkb = {
        layout = lib.mkDefault "es";
        options = lib.mkDefault "ctrl:nocaps";
      };
      console.useXkbConfig = lib.mkDefault true;
    })

    # only when the host uses home-manager as a NixOS module: HM uses the
    # system pkgs (same nixpkgs, allowUnfree, overlays) instead of its own
    (lib.optionalAttrs (options ? home-manager) (
      lib.mkIf cfg.enable {
        home-manager.useGlobalPkgs = true;
      }
    ))

    # prebuilt nix-index db (nix-locate, command-not-found, comma). The module
    # is imported by the flake for every host; base decides if it is on.
    (lib.optionalAttrs (lib.hasAttrByPath [ "programs" "nix-index-database" ] options) {
      programs.nix-index-database = {
        enable = cfg.enable;
        comma.enable = cfg.enable;
      };
    })
  ];
}
