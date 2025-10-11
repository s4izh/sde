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
    "${modules}/dwm.nix"
    "${modules}/hyprland.nix"
    "${modules}/river.nix"
    "${modules}/nvim.nix"
    "${modules}/virtualisation.nix"
    "${modules}/guix.nix"
    inputs.home-manager.nixosModules.home-manager
    inputs.nixos-hardware.nixosModules.asus-battery
  ];

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

  # specialisation = {
  #   silvia.configuration = {
  #     system.nixos.tags = [ "silvia" ];
  #     users.users.silvia = {
  #       isNormalUser = true;
  #       extraGroups = [
  #         "networkmanager"
  #         "video"
  #       ];
  #       initialPassword = "<PLACEHOLDER_FOR_PASSWORD>";
  #     };
  #     services.xserver = {
  #       enable = true;
  #       desktopManager.gnome.enable = true;
  #       displayManager.gdm.enable = true;
  #     };
  #     # services.displayManager.defaultSession = "xfce";
  #     services.tlp.enable = lib.mkForce false;
  #     environment.systemPackages = with pkgs; [
  #       firefox
  #       google-chrome
  #       chromium
  #       alacritty
  #       gnome-tweaks
  #     ];
  #   };
  # };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11";
}
