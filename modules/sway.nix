{
  config,
  pkgs,
  lib,
  ...
}: {
  # services.xserver = {
  #   enable = true;
  #   windowManager.stumpwm.enable = true;
  #   displayManager.lightdm.enable = true;
  # };

  programs.sway.enable = true;
  programs.waybar.enable = true;

  environment.systemPackages = with pkgs; [
    slurp
    grim
  ];
}
