{
  config,
  pkgs,
  lib,
  ...
}:
{
  # services.xserver = {
  #   enable = true;
  #   windowManager.stumpwm.enable = true;
  #   displayManager.lightdm.enable = true;
  # };

  programs.river-classic = {
    enable = true;
  };

  environment.systemPackages = with pkgs; [ yambar ];
}
