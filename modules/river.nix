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

  programs.river = {
    enable = true;
  };

  environment.systemPackages = with pkgs; [
    yambar
  ];
}
