{
  description = "Flake for my multisystem NixOS configuration";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ self, nixpkgs, home-manager, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };
      lib = nixpkgs.lib;
    in
    {
      nixosConfigurations = {
        z390 = lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = { inherit inputs; };
          modules = [
            ./machines/z390/configuration.nix
            ./modules/base.nix
            ./modules/desktop.nix
            ./modules/dwm.nix
            ./modules/gaming.nix
            ./modules/virtualisation.nix
            ./modules/android.nix
            home-manager.nixosModules.home-manager
            {
              home-manager = {
                useGlobalPkgs = true;
                extraSpecialArgs = { inherit inputs; };
                users.sergio.imports = [
                  ./home/sergio/home.nix
                ];
              };
            }
          ];
        };
        zen = lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            ./machines/zen/configuration.nix
            ./modules/base.nix
            ./modules/desktop.nix
            ./modules/dwm.nix
            ./modules/virtualisation.nix
            home-manager.nixosModules.home-manager
            {
              home-manager = {
                useGlobalPkgs = true;
                extraSpecialArgs = { inherit inputs; };
                users.sergio.imports = [
                  ./home/sergio/home.nix
                ];
              };
            }
          ];
        };
        # TODO specific vm config
        vm = lib.nixosSystem {
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
                users.sergio.imports = [
                  ./home/sergio/home.nix
                ];
              };
            }
          ];
        };
      };
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
