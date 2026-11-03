# Dell Latitude 7490: work laptop. Same as rx minus gaming and ollama.
{
  inputs,
  config,
  pkgs,
  sde,
  ...
}:
{
  imports = [
    ./hardware-configuration.nix
    ./configuration.nix
    inputs.home-manager.nixosModules.home-manager
  ];

  sde = {
    base.enable = true;
    desktop.enable = true;
    dev.enable = true;
    nvim.enable = true;
    vpn.enable = true;
    wm.mango.enable = true;
    virtualization.docker.enable = true;
    virtualization.podman.enable = true;
    latex.enable = true;
    work.bsc.enable = true;
  };

  home-manager.sharedModules = [
    {
      sde.theming.theme = "breeze";
      sde.autostart.remove = [ "discord" ];
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
