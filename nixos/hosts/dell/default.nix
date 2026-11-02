# Dell Latitude 7490: work laptop. Same as rx minus gaming and ollama.
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
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./configuration.nix
    "${modules}/base.nix"
    "${modules}/desktop.nix"
    "${modules}/dev.nix"
    "${modules}/nvim.nix"
    "${modules}/vpn.nix"
    "${wm}/mango.nix"
    inputs.home-manager.nixosModules.home-manager
  ];

  sde.desktop.enable = true;
  sde.virtualization.docker.enable = true;
  sde.virtualization.podman.enable = true;
  sde.latex.enable = true;
  sde.work.bsc.enable = true;

  home-manager.sharedModules = [
    {
      sde.theming.theme = "breeze";
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
}
