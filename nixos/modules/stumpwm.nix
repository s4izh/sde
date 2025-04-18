{
  config,
  pkgs,
  lib,
  ...
}:
{
  services.xserver = {
    enable = true;
    windowManager.stumpwm.enable = true;
    displayManager.lightdm.enable = true;
  };

  environment.systemPackages = with pkgs; [
    sxhkd
    stumpwm
    stumpish
    # lisp-stumpwm
  ];
}
