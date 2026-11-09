{
  inputs,
  config,
  pkgs,
  ...
}:
{
  imports = [
    ./hardware-configuration.nix
    ./configuration.nix
    inputs.home-manager.nixosModules.home-manager
  ];

  sde = {
    base.enable = true;
    desktop.enable = true;
  };

  home-manager = {
    extraSpecialArgs = {
      inherit inputs;
    };
    users = {
      sergio = import ../../home/sergio/home.nix;
    };
  };

  system.stateVersion = "23.11";
}
