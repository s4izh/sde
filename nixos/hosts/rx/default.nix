# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  inputs,
  config,
  pkgs,
  sde,
  ...
}:
let
  modules = ../../modules;
  wm = ../../modules/wm;
in
{
  system.stateVersion = "23.11";

  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./ollama.nix
    ./server.nix
    "${modules}/base.nix"
    "${modules}/desktop.nix"
    "${modules}/gaming.nix"
    "${modules}/android.nix"
    "${modules}/dev.nix"
    "${modules}/nvim.nix"
    # "${modules}/gnome.nix"
    # "${modules}/kde.nix"

    # "${wm}/river.nix"
    "${wm}/mangowc.nix"
    "${modules}/vpn.nix"
    inputs.home-manager.nixosModules.home-manager
  ];

  sde.desktop.enable = true;
  sde.virtualization.docker.enable = true;
  sde.virtualization.podman.enable = true;
  sde.latex.enable = true;

  home-manager = {
    extraSpecialArgs = {
      inherit inputs sde;
    };
    users = {
      sergio = import ../../home/sergio/home.nix;
    };
  };

  networking.hostName = "rx";
  networking.networkmanager.enable = true;

  boot.loader.systemd-boot = {
    enable = true;
    configurationLimit = 5;
    windows = {
      "10" = {
        title = "Windows 10";
        efiDeviceHandle = "hd0b32768a1";
      };
    };
  };

  boot.loader.systemd-boot.edk2-uefi-shell.enable = true;

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
  # hardware.keyboard.qmk.enable = true;

  services.hardware.openrgb.enable = true;

  # Optionally, you may need to select the appropriate driver version for your specific GPU.
  # hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.stable;

  # nvidia-drm.modeset=1 is required for some wayland compositors, e.g. sway
  # hardware.nvidia.modesetting.enable = true;

  services.scx = {
    enable = true;
    # lattency aware virtual deadline steam deck scheduler
    # scheduler = "scx_lavd";
    # scheduler = "scx_bore";
  };

  hardware.i2c.enable = true;

  users.users.sergio = {
    extraGroups = [
      "i2c"
    ];
    packages = with pkgs; [ ddcutil ];
  };
}
