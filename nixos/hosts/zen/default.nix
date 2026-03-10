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
let
  modules = ../../modules;
  wm = ../../modules/wm;
in
{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./configuration.nix
    # ./battery.nix
    "${modules}/base.nix"
    "${modules}/desktop.nix"
    "${modules}/dev.nix"
    "${modules}/vpn.nix"
    # "${modules}/unity.nix"
    # "${modules}/gnome.nix"
    # "${wm}/dwm.nix"
    "${wm}/river.nix"
    "${wm}/mangowc.nix"
    "${modules}/nvim.nix"
    # "${modules}/guix.nix"
    inputs.home-manager.nixosModules.home-manager
    inputs.nixos-hardware.nixosModules.asus-battery
  ];

  sde.virtualization.docker.enable = true;

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
