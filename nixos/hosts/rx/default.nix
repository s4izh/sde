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
in
{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./configuration.nix
    "${modules}/base.nix"
    "${modules}/desktop.nix"
    # "${modules}/dwm.nix"
    # "${modules}/dwl.nix"
    "${modules}/hyprland.nix"
    "${modules}/gaming.nix"
    "${modules}/virtualisation.nix"
    "${modules}/android.nix"
    "${modules}/dev.nix"
    "${modules}/nvim.nix"
    "${modules}/river.nix"
    "${modules}/guix.nix"
    # "${modules}/sway.nix"
    # "${modules}/stumpwm.nix"
    "${modules}/vpn.nix"
    # "${modules}/xfce.nix"
    # "${modules}/i3.nix"
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

  services.ollama = {
    enable = true;
    acceleration = "rocm";
    loadModels = [
      "mistral"
      "mistral:instruct"
      "deepseek-r1:14b"
      "llama3"
    ];
    rocmOverrideGfx = "10.3.0";
  };

  services.open-webui = {
    enable = true;
    host = "0.0.0.0";
    port = 10000;
    environment = {
      ANONYMIZED_TELEMETRY = "False";
      DO_NOT_TRACK = "True";
      SCARF_NO_ANALYTICS = "True";
      OLLAMA_API_BASE_URL = "http://127.0.0.1:11434/api";
      OLLAMA_BASE_URL = "http://127.0.0.1:11434";
    };
  };

  networking.firewall.allowedTCPPorts = [ 8080 ];

  system.stateVersion = "23.11";
}
