{
  pkgs,
  lib,
  inputs,
  ...
}:
{
  environment.systemPackages = [ ];

  programs.adb.enable = true;
  users.users.sergio.extraGroups = [ "adbusers" ];
}
