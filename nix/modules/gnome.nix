{
  config,
  pkgs,
  lib,
  ...
}:
{
  services.xserver = {
    enable = true;
    desktopManager.gnome.enable = true;
    displayManager.gdm.enable = true;
  };

  sde.desktop.fullEnvironment = true;

  environment.systemPackages = with pkgs; [
  ];

  services.tlp.enable = false;
}
