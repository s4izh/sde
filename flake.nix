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
      nixosConfigurations = let
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
        ];
        autoMachineConfigs = map createMachineConfig machineNames;
        machineConfigs = autoMachineConfigs ++ [
        {
          name = "vm";
          value = lib.nixosSystem {
            system = "x86_64-linux";
            modules = [
                ./machines/zen/configuration.nix
                ./modules/base.nix
                ./modules/desktop.nix
                ./modules/dwm.nix
                home-manager.nixosModules.home-manager
                {
                  home-manager = {
                    useGlobalPkgs = true;
                    extraSpecialArgs = { inherit inputs; };
                    users.sergio.imports = [ ./home/sergio/home.nix ];
                  };
                }
            ];
          };
        }
        ];

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
