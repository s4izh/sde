# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      #./battery.nix
    ];

  networking.hostName = "zen";
  networking.networkmanager.enable = true;

  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.configurationLimit = 5;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";

  #services.tlp.enable = true;

  services.tlp = {
    enable = true;
    settings = {
#      SOUND_POWER_SAVE_ON_AC = 1;
#      SOUND_POWER_SAVE_ON_BAT = 1;
#      RUNTIME_PM_ON_AC = "auto";
      PCIE_ASPM_ON_AC = "powersave";
      PCIE_ASPM_ON_BAT = "powersave";
    };
  };

  environment.systemPackages = with pkgs; [
    powertop
    acpi
    brightnessctl
  ];

  system.stateVersion = "22.11";
}
