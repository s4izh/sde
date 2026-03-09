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
  enableOllama = false;
in
{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    "${modules}/base.nix"
    "${modules}/desktop.nix"
    # "${modules}/dwm.nix"
    # "${modules}/dwl.nix"
    # "${modules}/hyprland.nix"
    "${modules}/gaming.nix"
    "${modules}/virtualisation.nix"
    "${modules}/android.nix"
    "${modules}/dev.nix"
    "${modules}/nvim.nix"

    "${wm}/river.nix"
    "${wm}/mangowc.nix"
    # "${wm}/gnome.nix"
    # "${wm}/sway.nix"
    # "${wm}/stumpwm.nix"
    # "${wm}/i3.nix"
    # "${wm}/xfcei3.nix"

    "${modules}/vpn.nix"
    # "${modules}/guix.nix"
    inputs.home-manager.nixosModules.home-manager
    # inputs.minegrub-world-sel-theme.nixosModules.default
  ];

  sde.desktop.enable = true;

  home-manager = {
    extraSpecialArgs = {
      inherit inputs sde;
    };
    users = {
      sergio = import ../../home/sergio/home.nix;
    };
  };

  #environment.systemPackages = with pkgs; [
  #  ollama-rocm
  #  (writeShellScriptBin "ollama-serve-rocm" ''
  #    #!/usr/bin/env bash
  #    HSA_OVERRIDE_GFX_VERSION=10.3.0 ollama serve
  #  '')
  #  alpaca
  #];

  nixpkgs.config.rocmSupport = enableOllama;

  services.ollama = {
    package = pkgs.ollama-rocm;
    enable = enableOllama;
    host = "0.0.0.0";
    port = 11434;
    loadModels = [
      "gemma2:9b"
      "qwen2.5-coder:7b"
      "qwen2.5-coder:14b"
    ];
    rocmOverrideGfx = "10.3.0";
  };

  services.open-webui = {
    enable = enableOllama;
    host = "0.0.0.0";
    port = 10000;
    environment = {
      WEBUI_AUTH = "False";
      ANONYMIZED_TELEMETRY = "False";
      DO_NOT_TRACK = "True";
      SCARF_NO_ANALYTICS = "True";

      # 1. Disable Gravatar (Stops sending your email hash to Gravatar servers)
      ENABLE_GRAVATAR = "False";
      # 2. Disable Update Checks (Stops pinging GitHub/Home to check for new versions)
      CHECK_UPDATES = "False";
      # 3. Disable Community Sharing (Prevents accidental sharing of prompts/tools to the public site)
      ENABLE_COMMUNITY_SHARING = "False";
      # 4. Security: Disable New Signups (Crucial!)
      # Once you have recovered your account, set this to False.
      # This prevents anyone else on your network from creating an account on your LLM.
      ENABLE_SIGNUP = "False";

      OLLAMA_API_BASE_URL = "http://127.0.0.1:11434/api";
      OLLAMA_BASE_URL = "http://127.0.0.1:11434";
    };
  };

  networking.firewall = {
    enable = true;
    allowedTCPPorts = [
      10000
      11434
    ];
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

  system.stateVersion = "23.11";
}
