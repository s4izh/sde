{ inputs, config, pkgs, ... }:

let modules = ../../modules;

in {
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./configuration.nix
    "${modules}/base.nix"
    "${modules}/desktop.nix"
    "${modules}/dwm.nix"
    inputs.home-manager.nixosModules.home-manager
  ];

  home-manager = {
    extraSpecialArgs = { inherit inputs; };
    users = { sergio = import ../../home/sergio/home.nix; };
  };

  system.stateVersion = "23.11";
}
