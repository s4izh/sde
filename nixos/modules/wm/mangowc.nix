{
  config,
  pkgs,
  lib,
  ...
}:
{
  programs.mangowc.enable = true;

  # TODO: extract into wayland common
  xdg.portal.wlr.enable = true;
  xdg.portal.extraPortals = [
    pkgs.xdg-desktop-portal-gtk
    pkgs.xdg-desktop-portal-wlr
  ];

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
    lswt # window info
    waylock
    wmenu
    foot

    # dms shell packages
    dms-shell
    quickshell
    cava # audio viewer
    dgop # process status
    matugen # color generator
    dsearch # cli file searcher
  ];

  # needed for waylock to work
  security.pam.services.waylock = { };

  environment.sessionVariables.MOZ_ENABLE_WAYLAND = "1";
  environment.sessionVariables.NIXOS_OZONE_WL = "1";
}
