{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.sde.wm.mango;

  mango-launcher2 = pkgs.writeShellScriptBin "launch-desktop-session.sh" ''
    #!/usr/bin/env bash

    export XDG_SESSION_TYPE=wayland
    export XDG_CURRENT_DESKTOP=mango
    export XDG_SESSION_DESKTOP=mango

    systemctl --user import-environment \
    DISPLAY WAYLAND_DISPLAY XDG_CURRENT_DESKTOP NIXOS_OZONE_WL \
    MOZ_ENABLE_WAYLAND XDG_SESSION_DESKTOP XDG_SESSION_TYPE \
    XCURSOR_THEME XCURSOR_SIZE

    if command -v dbus-update-activation-environment >/dev/null 2>&1; then
        dbus-update-activation-environment \
          DISPLAY WAYLAND_DISPLAY XDG_CURRENT_DESKTOP NIXOS_OZONE_WL \
          MOZ_ENABLE_WAYLAND XDG_SESSION_DESKTOP XDG_SESSION_TYPE \
          XCURSOR_THEME XCURSOR_SIZE
    fi

    exec ${pkgs.mangowc}/bin/mango &> /tmp/mango.log
  '';

  mango-sync-env = pkgs.writeShellScriptBin "mango-sync-env" ''
    #!/usr/bin/env bash
    while [ ! -S "$XDG_RUNTIME_DIR/wayland-0" ] && [ ! -S "$XDG_RUNTIME_DIR/wayland-1" ]; do
      sleep 0.5
    done

    W_DISPLAY=$(ls $XDG_RUNTIME_DIR/wayland-* | head -n 1 | xargs basename)

    systemctl --user import-environment DISPLAY WAYLAND_DISPLAY XDG_CURRENT_DESKTOP
    dbus-update-activation-environment --systemd WAYLAND_DISPLAY=$W_DISPLAY XDG_CURRENT_DESKTOP=mango

    systemctl --user start mango-session.target
    systemctl --user restart xdg-desktop-portal xdg-desktop-portal-wlr xdg-desktop-portal-gtk
  '';

  mango-launcher = pkgs.writeShellScriptBin "launch-desktop-session.sh" ''
    #!/usr/bin/env bash
    export XDG_SESSION_TYPE=wayland
    export XDG_CURRENT_DESKTOP=mango
    export XDG_SESSION_DESKTOP=mango

    ${mango-sync-env}/bin/mango-sync-env &
    exec ${pkgs.mangowc}/bin/mango &> /tmp/mango.log
  '';
in
{
  options.sde.wm.mango = {
    enable = lib.mkEnableOption "Mango Wayland compositor session";
  };

  config = lib.mkIf cfg.enable {
    programs.mango.enable = true;

    systemd.user.targets.mango-session = {
      description = "mango compositor session";
      bindsTo = [ "graphical-session.target" ];
      wants = [ "graphical-session-pre.target" ];
      after = [ "graphical-session-pre.target" ];
    };

    systemd.user.services.polkit-gnome-agent = {
      description = "polkit authentication agent";
      wantedBy = [ "mango-session.target" ];
      partOf = [ "mango-session.target" ];
      after = [ "mango-session.target" ];
      serviceConfig = {
        ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
        Restart = "on-failure";
      };
    };

    security.polkit.enablePkexecWrapper = true;
    services.gnome.gnome-keyring.enable = true;

    xdg.portal = {
      enable = true;
      wlr.enable = true;
      extraPortals = [
        pkgs.xdg-desktop-portal-gtk
        pkgs.xdg-desktop-portal-wlr
      ];
      config = {
        common = {
          default = [ "gtk" ];
          "org.freedesktop.impl.portal.ScreenCast" = "wlr";
          "org.freedesktop.impl.portal.Screenshot" = "wlr";
        };
        mango = {
          default = [ "gtk" ];
          "org.freedesktop.impl.portal.ScreenCast" = "wlr";
          "org.freedesktop.impl.portal.Screenshot" = "wlr";
        };
      };
    };

    environment.sessionVariables = {
      MOZ_ENABLE_WAYLAND = "1";
      NIXOS_OZONE_WL = "1";
    };

    environment.systemPackages = with pkgs; [
      wl-clipboard
      bemenu
      swayosd
      swaynotificationcenter
      playerctl
      slurp
      grim
      kanshi
      wlr-randr
      wdisplays
      wpaperd
      awww
      swaybg
      lswt
      waylock
      swaylock
      swayidle
      wmenu
      foot
      nsxiv
      wev
      dux-gop
      mango-launcher
    ];

    security.pam.services.waylock = { };
    security.pam.services.swaylock = { };

    services.cron = {
      enable = true;
      systemCronJobs = [
        "0 8 * * *  sergio  export XDG_RUNTIME_DIR=/run/user/1000 WAYLAND_DISPLAY=wayland-0; /home/sergio/.local/scripts/gm --level 1"
        "0 18 * * * sergio  export XDG_RUNTIME_DIR=/run/user/1000 WAYLAND_DISPLAY=wayland-0; /home/sergio/.local/scripts/gm --level 3"
        "0 19 * * * sergio  export XDG_RUNTIME_DIR=/run/user/1000 WAYLAND_DISPLAY=wayland-0; /home/sergio/.local/scripts/gm --level 4"
        "0 21 * * * sergio  export XDG_RUNTIME_DIR=/run/user/1000 WAYLAND_DISPLAY=wayland-0; /home/sergio/.local/scripts/gm --level 5"
        "0 22 * * * sergio  export XDG_RUNTIME_DIR=/run/user/1000 WAYLAND_DISPLAY=wayland-0; /home/sergio/.local/scripts/gm --level 6"
        "0 23 * * * sergio  export XDG_RUNTIME_DIR=/run/user/1000 WAYLAND_DISPLAY=wayland-0; /home/sergio/.local/scripts/gm --level 7"
      ];
    };
  };
}
