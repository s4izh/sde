{
  config,
  pkgs,
  lib,
  ...
}:
let
  river-launcher = pkgs.writeShellScriptBin "launch-desktop-session.sh" ''
    #!/usr/bin/env bash

    export XDG_SESSION_TYPE=wayland
    export XDG_CURRENT_DESKTOP=river
    export XDG_SESSION_DESKTOP=river

    # exec ${pkgs.river}/bin/river &> /tmp/river.log

    systemctl --user import-environment DISPLAY XAUTHORITY
    if command -v dbus-update-activation-environment >/dev/null 2>&1; then
        dbus-update-activation-environment DISPLAY XAUTHORITY
    fi
    gnome-keyring-daemon --start --components=secrets

    dbus-launch --exit-with-session ${pkgs.river}/bin/river 2> /tmp/river.log
  '';

in
{
  programs.river = {
    enable = true;
  };

  services.xserver.displayManager.gdm.enable = true;
  services.xserver.displayManager.gdm.wayland = true;

  xdg.portal.wlr.enable = true;
  xdg.portal.extraPortals = [
    pkgs.xdg-desktop-portal-gtk
    pkgs.xdg-desktop-portal-wlr
  ];

  environment.systemPackages =
    with pkgs;
    [
      wl-clipboard
      bemenu
      swayosd
      swaynotificationcenter
      playerctl
      slurp
      grim
      kanshi
    ]
    ++ [ river-launcher ];

  environment.sessionVariables.MOZ_ENABLE_WAYLAND = "1";
  environment.sessionVariables.NIXOS_OZONE_WL = "1";
}
