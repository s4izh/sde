{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.sde.desktop;
in
{
  imports = [ ];

  options = {
    sde.desktop.enable = lib.mkEnableOption "Enable SDE desktop";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      guile
    ];
  };
}
