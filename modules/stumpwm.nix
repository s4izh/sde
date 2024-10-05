
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
    stumpwm
    # lisp-stumpwm
  ];

  nixpkgs.overlays = [
    (final: prev: {
      dmenu = prev.dmenu.overrideAttrs (old: {
        src = /home/sergio/.local/src/dmenu;
      });
  })
  ];
}
