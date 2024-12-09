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

    exec ${pkgs.river}/bin/river &> /tmp/river.log
  '';

in
{
  programs.river = {
    enable = true;
  };

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
}
