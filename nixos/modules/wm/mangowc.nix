{
  config,
  pkgs,
  lib,
  ...
}:
let
  mango-launcher2 = pkgs.writeShellScriptBin "launch-desktop-session.sh" ''
    #!/usr/bin/env bash

    export XDG_SESSION_TYPE=wayland
    export XDG_CURRENT_DESKTOP=mango
    export XDG_SESSION_DESKTOP=mango

    # exec ${pkgs.river-classic}/bin/river &> /tmp/river.log

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
    # gnome-keyring-daemon --start --components=secrets

    # dbus-launch --exit-with-session ${pkgs.mangowc}/bin/mango &> /tmp/mango.log
    exec ${pkgs.mangowc}/bin/mango &> /tmp/mango.log
  '';

    mango-sync-env = pkgs.writeShellScriptBin "mango-sync-env" ''
      #!/usr/bin/env bash
      # Wait for the Wayland socket to actually appear in the filesystem
      while [ ! -S "$XDG_RUNTIME_DIR/wayland-0" ] && [ ! -S "$XDG_RUNTIME_DIR/wayland-1" ]; do
        sleep 0.5
      done

      # Detect which one was created
      W_DISPLAY=$(ls $XDG_RUNTIME_DIR/wayland-* | head -n 1 | xargs basename)

      # Inject the variables into the systemd and dbus environments
      systemctl --user import-environment DISPLAY WAYLAND_DISPLAY XDG_CURRENT_DESKTOP
      dbus-update-activation-environment --systemd WAYLAND_DISPLAY=$W_DISPLAY XDG_CURRENT_DESKTOP=mango

      # CRITICAL: Restart the portals now that the environment is valid
      systemctl --user restart xdg-desktop-portal xdg-desktop-portal-wlr xdg-desktop-portal-gtk
    '';

  mango-launcher = pkgs.writeShellScriptBin "launch-desktop-session.sh" ''
    #!/usr/bin/env bash
    export XDG_SESSION_TYPE=wayland
    export XDG_CURRENT_DESKTOP=mango
    export XDG_SESSION_DESKTOP=mango

    # Start the sync script in the background
    ${mango-sync-env}/bin/mango-sync-env &

    exec ${pkgs.mangowc}/bin/mango &> /tmp/mango.log
  '';
in
{
  programs.mangowc.enable = true;
  # programs.regreet.enable = true;
  # services.greetd.enable = true;

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
    swayosd # volume/brightness gui
    swaynotificationcenter # notifications
    playerctl # manage media
    slurp # screenshots
    grim # screenshots
    kanshi # manage monitors
    wlr-randr # manage monitors
    wpaperd # wallpapers
    swww # wallpapers
    swaybg # wallpapers
    lswt # window info
    waylock
    wmenu
    foot
    nsxiv # images
    wev # user input

    # dms shell packages
    # dms-shell
    # quickshell
    # cava # audio viewer
    dgop # process status
    # matugen # color generator
    # dsearch # cli file searcher

  ]
  ++ [ mango-launcher ];

  # needed for waylock to work
  security.pam.services.waylock = { };

  services.cron = {
    enable = true;
    systemCronJobs =[
      "0 8 * * *  sergio  export XDG_RUNTIME_DIR=/run/user/1000 WAYLAND_DISPLAY=wayland-0; /home/sergio/.local/scripts/gm --level 1"
      "0 18 * * * sergio  export XDG_RUNTIME_DIR=/run/user/1000 WAYLAND_DISPLAY=wayland-0; /home/sergio/.local/scripts/gm --level 3"
      "0 19 * * * sergio  export XDG_RUNTIME_DIR=/run/user/1000 WAYLAND_DISPLAY=wayland-0; /home/sergio/.local/scripts/gm --level 4"
      "0 21 * * * sergio  export XDG_RUNTIME_DIR=/run/user/1000 WAYLAND_DISPLAY=wayland-0; /home/sergio/.local/scripts/gm --level 5"
      "0 22 * * * sergio  export XDG_RUNTIME_DIR=/run/user/1000 WAYLAND_DISPLAY=wayland-0; /home/sergio/.local/scripts/gm --level 6"
      "0 23 * * * sergio  export XDG_RUNTIME_DIR=/run/user/1000 WAYLAND_DISPLAY=wayland-0; /home/sergio/.local/scripts/gm --level 7"
    ];
  };
}
