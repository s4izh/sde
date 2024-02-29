{
  description = "Flake for my multisystem NixOS configuration";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, nixpkgs, home-manager, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };
      lib = nixpkgs.lib;
    in {
      nixosConfigurations =
      let
        createMachineConfig = machineName: {
          name = "${machineName}";
          value = lib.nixosSystem {
            system = "x86_64-linux";
            specialArgs = { inherit inputs; };
            modules = [ ./machines/${machineName} ];
          };
        };
        machineNames = [ 
          "z390"
          "rx"
          "zen"
          "jsc"
          "vm"
        ];
        autoMachineConfigs = map createMachineConfig machineNames;

        machineConfigs = autoMachineConfigs ++ [ ];

        in
          builtins.listToAttrs machineConfigs;

      # };
      homeManagerConfigurations = {
        sergio = home-manager.lib.homeManagerConfiguration {
          # pkgs = nixpkgs.legacyPackages.${system};
          inherit pkgs;
          modules = [
            ./home/sergio/home.nix
            {
              home = {
                username = "sergio";
                homeDirectory = "/home/sergio";
                stateVersion = "23.05";
              };
            }
          ];
        };
      };
    };
}
