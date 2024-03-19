# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:
{
  networking.hostName = "zen";
  networking.networkmanager.enable = true;

  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  services.xserver.videoDrivers = [ "amdgpu" ];
  hardware.opengl.enable = true;

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.configurationLimit = 5;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot";

  services.tlp = {
    enable = true;
    settings = {
      PCIE_ASPM_ON_AC = "powersave";
      PCIE_ASPM_ON_BAT = "powersave";
    };
  };

  services.openssh = {
    enable = true;
    settings.PasswordAuthentication = false;
  };

  environment.systemPackages = with pkgs; [
    powertop
    acpi
    brightnessctl
  ];

  services.logind.extraConfig = ''
    HandlePowerKey=ignore
  '';

  hardware.asus.battery = {
    enableChargeUptoScript = true;
    chargeUpto = 85;
  };

  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 5900 ];
  };

}
