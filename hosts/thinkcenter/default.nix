{
  inputs,
  config,
  pkgs,
  ...
}:
let
  modules = ../../modules;
in
{
  imports = [
    ./hardware-configuration.nix
    ./configuration.nix
    ./minecraft.nix
  ];

  system.stateVersion = "23.11";
}
