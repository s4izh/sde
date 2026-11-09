# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  inputs,
  config,
  pkgs,
  sde,
  lib,
  ...
}:
{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./configuration.nix
    inputs.home-manager.nixosModules.home-manager
    inputs.nixos-hardware.nixosModules.asus-battery
  ];

  sde = {
    base.enable = true;
    desktop.enable = true;
    dev.enable = true;
    vpn.enable = true;
    wm.mango.enable = true;
    nvim.enable = true;
    virtualization.docker.enable = true;
  };

  home-manager = {
    extraSpecialArgs = {
      inherit inputs sde;
    };
    users = {
      sergio = import ../../home/sergio/home.nix;
    };
  };

  programs.localsend = {
    enable = true;
    openFirewall = true;
  };

  hardware.asus.battery = {
    chargeUpto = 85;
    enableChargeUptoScript = true;
  };

  system.stateVersion = "23.11";
}
