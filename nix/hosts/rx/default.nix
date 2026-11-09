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
{
  system.stateVersion = "23.11";

  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./ollama.nix
    ./server.nix
    inputs.home-manager.nixosModules.home-manager
  ];

  sde = {
    base.enable = true;
    desktop.enable = true;
    gaming.enable = true;
    android.enable = true;
    dev.enable = true;
    nvim.enable = true;
    music.enable = true;
    wm.mango.enable = true;
    vpn.enable = true;
    virtualization.docker.enable = true;
    virtualization.podman.enable = true;
    latex.enable = true;
    work.bsc.enable = true;
  };
  # themes: adwaita (default) breeze gruvbox nordic catppuccin tokyonight
  # colloid manjaro
  home-manager.sharedModules = [
    {
      # Hermes (ACP) + Multica daemon, see docs/agents.md
      sde.agents.enable = true;

      sde.theming.theme = "adwaita";
      # kitty theme per mode, a file in dotfiles/.config/kitty/themes without
      # .conf. dark = null keeps theme.conf
      # sde.theming.kitty.dark = "gruvbox-dark-hard";
      # sde.theming.kitty.light = "Modus_Operandi";

      # swaylock background, null (default) generates one in the theme accent
      # sde.theming.lockImage = "~/git/wallpapers/wallhaven-q6p667.jpg";
    }
  ];

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

  # per-link DNS (VPNs, Tailscale) on top of a global 8.8.8.8 fallback
  services.resolved.enable = true;
  networking.nameservers = [ "8.8.8.8" ];

  # monitor layout for plasma, same as the kanshi "horizontal" profile
  sde.kde.kscreenArgs = [
    "output.DP-3.mode.1920x1080@200"
    "output.DP-3.position.0,0"
    "output.HDMI-A-1.mode.1920x1080@200"
    "output.HDMI-A-1.position.1920,0"
    "output.HDMI-A-1.priority.1"
    "output.DP-1.mode.1920x1080@200"
    "output.DP-1.position.3840,0"
  ];

  services.xserver.videoDrivers = [ "amdgpu" ];

  hardware.graphics.enable = true;

  environment.systemPackages = with pkgs; [ amdgpu_top ];

  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
  };
  services.blueman.enable = true;

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
    # keep systemd --user (multica-daemon) running without a login session
    linger = true;
  };

  boot.extraModulePackages = [ config.boot.kernelPackages.v4l2loopback ];
  boot.kernelModules = [ "v4l2loopback" ];
  boot.extraModprobeConfig = ''
    options v4l2loopback devices=1 video_nr=10 card_label="FX Cam" exclusive_caps=1
    '';

}
