{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.sde.kde;
in
{
  options.sde.kde.kscreenArgs = lib.mkOption {
    type = lib.types.listOf lib.types.str;
    default = [ ];
    example = [
      "output.DP-1.mode.1920x1080@144"
      "output.DP-1.position.0,0"
    ];
    description = "kscreen-doctor arguments applied on every plasma login (monitor layout).";
  };

  config = lib.mkIf (config.services.desktopManager.plasma6.enable && cfg.kscreenArgs != [ ]) {
    environment.etc."xdg/autostart/kscreen-layout.desktop".text = ''
      [Desktop Entry]
      Type=Application
      Name=kscreen layout
      OnlyShowIn=KDE;
      NoDisplay=true
      Exec=${pkgs.kdePackages.libkscreen}/bin/kscreen-doctor ${lib.concatStringsSep " " cfg.kscreenArgs}
    '';
  };
}
