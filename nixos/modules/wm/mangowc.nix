{
  config,
  pkgs,
  lib,
  ...
}:
{
  # services.xserver = {
  #   enable = true;
  #   displayManager.startx.enable = true;
  # };

  environment.systemPackages = with pkgs; [
    mangowc
  ];
}
