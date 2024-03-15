{ config, pkgs, lib, ... }:
{
  services.xserver = {
    enable = true;
    # windowManager.dwm.enable = true;
    displayManager.startx.enable = true;
  };

  environment.systemPackages = with pkgs; [
    dwl
    somebar
    bemenu
    somebar
    foot
    pkg-config
  ];

  nixpkgs.overlays = [
    (final: prev: {
      dwl = prev.dwl.overrideAttrs (old: {
	      src = /home/sergio/.local/src/dwl ;
      });
      somebar = prev.somebar.overrideAttrs (old: {
	      src = /home/sergio/.local/src/somebar ;
      });
  })
  ];

  # system.stateVersion = "22.11"; # Did you read the comment? yes
}
