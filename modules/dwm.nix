{ config, pkgs, lib, ... }:
{
  services.xserver = {
    enable = true;
    windowManager.dwm.enable = true;
    displayManager.startx.enable = true;
  };

  environment.systemPackages = with pkgs; [
    dwm
    dwmblocks
    dmenu
    sxhkd
    xorg.xwininfo
    # st
    # harfbuzz
  ];

  nixpkgs.overlays = [
    (final: prev: {
      dwm = prev.dwm.overrideAttrs (old: {
	      src = /home/sergio/.local/src/dwm ;
      });
      dmenu = prev.dmenu.overrideAttrs (old: {
        src = /home/sergio/.local/src/dmenu ;
      });
      dwmblocks = prev.dwmblocks.overrideAttrs (old: {
        src = /home/sergio/.local/src/dwmblocks ;
      });
  })
  ];

  system.stateVersion = "22.11"; # Did you read the comment? yes
}
