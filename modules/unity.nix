{
  config,
  pkgs,
  lib,
  ...
}:
{
  environment.systemPackages = [
    pkgs.unityhub
  ];
  xdg.portal = {
    enable = true;
    xdgOpenUsePortal = true;
  };
}
