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
    gaming.enable = true;
    android.enable = true;
    virtualization.docker.enable = true;
  };

  home-manager = {
    extraSpecialArgs = {
      inherit inputs;
    };
    users = {
      sergio = import ../../home/sergio/home.nix;
    };
  };

  system.stateVersion = "23.05";
}
