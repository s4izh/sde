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

    dbus-launch --exit-with-session ${pkgs.river}/bin/river &> /tmp/river.log
  '';

in
{
  programs.river = {
    enable = true;
  };

  # systemd.user.services.xdg-desktop-portal-wlr = {
  #   enable = true;
  # };

  # environment.sessionVariables.WAYLAND_DISPLAY = "wayland-1";

  # services.xserver.displayManager.gdm.enable = true;
  # services.xserver.displayManager.gdm.wayland = true;

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
      swayosd # volume/brightness gui
      swaynotificationcenter # notifications
      playerctl # manage media
      slurp # screenshots
      grim # screenshots
      kanshi # manage monitors
      wpaperd # wallpapers
      lswt # window info
    ]
    ++ [ river-launcher ];

  environment.sessionVariables.MOZ_ENABLE_WAYLAND = "1";
  environment.sessionVariables.NIXOS_OZONE_WL = "1";
}
