{
  config,
  options,
  pkgs,
  lib,
  sde,
  ...
}:
let
  cfg = config.sde.desktop;
  hostfile = "${sde.flakeRoot}/dotfiles/etc/hosts";
  focusHostfile = "${sde.flakeRoot}/dotfiles/etc/hosts-focus";
in
{
  options = {
    sde.desktop.enable = lib.mkEnableOption "Enable SDE desktop";

    sde.desktop.fullEnvironment = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        Whether a full desktop environment (Plasma, GNOME) is in use. Those
        manage GTK theme and cursor themselves, so the home-manager theming
        (sde.theming in home-manager) gets disabled to avoid fighting them.
      '';
    };
  };

  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      environment.systemPackages = with pkgs; [
        guile
        alacritty
        kitty
        discord
        whatsapp-electron
        dmenu
        w3m
        pavucontrol
        pulsemixer
        zathura
        sioyek
        feh
        redshift
        networkmanagerapplet
        gnome-calculator
        gnome-keyring
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
        headsetcontrol
        sxiv
        htop
        resources
        x11vnc
        nodejs
        libtool
        python3
        starship
        nmap
        netscanner
        xdg-ninja
        pandoc
        haskellPackages.pandoc-crossref
        groff
        aspell
        obsidian
        marksman
        libreoffice-stable
        rclone
        pdftk
        libnotify
        newsboat
        cointop
        screenkey
        sdcv
        pywal
        baobab
        yazi
        ueberzugpp
        chromium
        google-chrome
        aria2
        wtype
        tree-sitter
        resources
        numbat
        sde.pkgs.${pkgs.stdenv.hostPlatform.system}.zen-browser
        graphviz
        ghostscript
        thunderbird
      ];

      programs.nix-ld.enable = true;
      services.envfs.enable = true;

      programs.firefox = {
        enable = true;
        package = pkgs.firefox;
        preferences = {
          "browser.fullscreen.autohide" = false;
          "browser.compactmode.show" = true;
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
          "sidebar.verticalTabs" = true;
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

      environment.sessionVariables._JAVA_AWT_WM_NONREPARENTING = "1";

      programs.npm.enable = true;

      programs.thunar.enable = true;
      programs.thunar.plugins = with pkgs; [
        thunar-media-tags-plugin
        thunar-archive-plugin
        thunar-volman
      ];

      services.gvfs.enable = true;
      services.dbus.enable = true;
      services.printing.enable = true;

      services.pulseaudio.enable = false;
      security.rtkit.enable = true;
      services.pipewire = {
        enable = true;
        alsa.enable = true;
        alsa.support32Bit = true;
        pulse.enable = true;
        wireplumber.enable = true;
      };

      networking.extraHosts = builtins.readFile hostfile;

      environment.etc."hosts.default".source = pkgs.concatText "hosts-default" config.networking.hostFiles;
      environment.etc."hosts.focus".source = pkgs.concatText "hosts-focus" (
        config.networking.hostFiles ++ [ focusHostfile ]
      );
      environment.etc.hosts.source = lib.mkForce "/etc/hosts.current";

      system.activationScripts.hostsCurrent.text = ''
        if [ ! -e /etc/hosts.current ]; then
          ln -sfn /etc/hosts.default /etc/hosts.current
        fi
      '';

      programs.ssh = {
        enableAskPassword = true;
        askPassword = "${pkgs.seahorse}/libexec/seahorse/ssh-askpass";
      };

      xdg.portal.enable = true;
      programs.dconf.enable = true;
    })

    # only when the host uses home-manager as a NixOS module
    (lib.optionalAttrs (options ? home-manager) {
      home-manager.sharedModules = [
        { sde.theming.enable = lib.mkDefault (!cfg.fullEnvironment); }
      ];
    })
  ];
}
