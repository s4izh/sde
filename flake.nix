{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    #home.manager.url = "github:nix-community/home-manager";
    #home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ self, nixpkgs, home-manager, ... }:
    let 
      pkgs = import nixpkgs { 
        #inherit system;
        config.allowUnfree = true;
      };
      lib = nixpkgs.lib;
    in {
      nixosConfigurations = {
        z390 = lib.nixosSystem {
          system = "x86_64-linux";
          modules = [ 
            ./machines/z390/configuration.nix
            ./modules/gaming.nix
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
    };
}
    
