# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  inputs,
  config,
  pkgs,
  ...
}:
{
  networking.hostName = "z390"; # Define your hostname.
  networking.networkmanager.enable = true;

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.configurationLimit = 5;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot";

  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 25565 ];
  };

  # networking = {
  #   interfaces.eno2.ipv4.addresses = [{
  #     address = "192.168.1.100";
  #     prefixLength = 24;
  #   }];
  # };

  services.xserver.videoDrivers = [ "nvidia" ];

  hardware.opengl.enable = true;

  # Optionally, you may need to select the appropriate driver version for your specific GPU.
  hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.stable;

  # nvidia-drm.modeset=1 is required for some wayland compositors, e.g. sway
  hardware.nvidia.modesetting.enable = true;
}
