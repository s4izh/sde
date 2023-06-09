{ config, pkgs, lib, ... }:
{
  services.xserver = {
    enable = true;
    desktopManager = {
      gnome =
      {
        enable = true;
      };
    };
  };

  environment.systemPackages = with pkgs; [
    dmenu
    xorg.xwininfo
  ];

  nixpkgs.overlays = [
    (final: prev: {
     dmenu = prev.dmenu.overrideAttrs (old: {
         src = /home/sergio/.local/src/dmenu ;
         });
     })
  ];
}
