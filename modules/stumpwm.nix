
{ config, pkgs, lib, ... }:
{
  services.xserver = {
    enable = true;
    windowManager.stumpwm.enable = true;
    displayManager.startx.enable = true;
  };

  environment.systemPackages = with pkgs; [
    dmenu
    sxhkd
  ];

  nixpkgs.overlays = [
    (final: prev: {
      dmenu = prev.dmenu.overrideAttrs (old: {
        src = /usr/local/src/dmenu ;
      });
  })
  ];

  system.stateVersion = "22.11"; # Did you read the comment? yes
}
