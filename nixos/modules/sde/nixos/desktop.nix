{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.sde.services.desktop;
in
{
  imports = [];

  options = {
    cfg.enable = lib.mkEnableOption "Enable SDE desktop";
  };

  config = {
    environment.systemPackages = lib.mkIf cfg.enable [
      pkgs.helix
    ];
  };
}
