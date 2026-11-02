{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.sde.android;
in
{
  options.sde.android = {
    enable = lib.mkEnableOption "Android tools and adbusers group";
  };

  config = lib.mkIf cfg.enable {
    users.users.sergio.extraGroups = [ "adbusers" ];
  };
}
