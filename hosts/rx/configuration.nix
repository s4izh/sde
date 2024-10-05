# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  inputs,
  config,
  pkgs,
  ...
}:
let
  fallout = pkgs.fetchFromGitHub {
    owner = "shvchk";
    repo = "fallout-grub-theme";
    rev = "80734103d0b48d724f0928e8082b6755bd3b2078";
    sha256 = "sha256-7kvLfD6Nz4cEMrmCA9yq4enyqVyqiTkVZV5y4RyUatU=";
  };
in
{
  networking.hostName = "rx"; # Define your hostname.
  networking.networkmanager.enable = true;

  # Bootloader.
  # boot.loader.systemd-boot.enable = true;
  # boot.loader.systemd-boot.configurationLimit = 5;

  boot.loader.efi.efiSysMountPoint = "/boot";
  boot.loader.efi.canTouchEfiVariables = true;

  boot.loader.grub = {
    enable = true;
    efiSupport = true;
    device = "nodev";
    configurationLimit = 5;
    useOSProber = true;
    # theme = fallout;
  };

  boot.loader.grub = {
    minegrub-world-sel = {
      enable = false;
      customIcons = [
        {
          name = "nixos";
          lineTop = "NixOS (23/11/2023, 23:03)";
          lineBottom = "Survival Mode, No Cheats, Version: 23.11";
          # Icon: you can use an icon from the remote repo, or load from a local file
          imgName = "nixos";
          customImg = builtins.path {
            path = ./../../extra/nixos-logo.png;
            name = "nixos";
          };
        }
      ];
    };
  };

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

  services.openssh = {
    enable = true;
  };

  environment.etc = {
    "resolv.conf".text = "nameserver 8.8.8.8\n";
  };

  services.xserver.videoDrivers = [ "amdgpu" ];

  hardware.graphics.enable = true;

  environment.systemPackages = with pkgs; [ amdgpu_top ];

  hardware.cpu.amd.updateMicrocode = true;
  hardware.keyboard.qmk.enable = true;

  # Optionally, you may need to select the appropriate driver version for your specific GPU.
  # hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.stable;

  # nvidia-drm.modeset=1 is required for some wayland compositors, e.g. sway
  # hardware.nvidia.modesetting.enable = true;
}
