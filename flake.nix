{
  description = "Flake for my multisystem NixOS configuration";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    home-manager,
    ...
  }: let
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      inherit system;
      config.allowUnfree = true;
    };
    overlays = [inputs.neovim-nightly-overlay.overlay];
    lib = nixpkgs.lib;
  in {
    nixosConfigurations = let
      # specialArgs = { inherit inputs; };
      # extraSpecialArgs = { inherit inputs; };
      mkHostConfig = {
        host,
        arch,
      }: {
        name = host;
        value = lib.nixosSystem {
          system = arch;
          specialArgs = {inherit inputs;};
          modules = [
            ./hosts/${host}
            # { nixpkgs.overlays = overlays; }
          ];
        };
      };
      hosts = [
        {
          host = "z390";
          arch = "x86_64-linux";
        }
        {
          host = "rx";
          arch = "x86_64-linux";
        }
        {
          host = "zen";
          arch = "x86_64-linux";
        }
        {
          host = "jsc";
          arch = "x86_64-linux";
        }
        {
          host = "vm";
          arch = "x86_64-linux";
        }
      ];
      autoMachineConfigs = map mkHostConfig hosts;

      machineConfigs = autoMachineConfigs ++ [];
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
              stateVersion = "23.11";
            };
          }
          {
            nixpkgs.overlays = overlays;
          }
        ];
      };
    };
  };
}
