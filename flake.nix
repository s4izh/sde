{
  description = "Flake for my multisystem NixOS configuration";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    minegrub-world-sel-theme = {
      url = "github:Lxtharia/minegrub-world-sel-theme";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      home-manager,
      ...
    }:
    let
      lib = nixpkgs.lib;

      forAllSystems =
        function:
        lib.genAttrs
          [
            "x86_64-linux"
            "aarch64-linux"
          ]
          (
            system:
            let
              syspkgs = import nixpkgs {
                inherit system;
                config.allowUnfree = true;
              };
            in
            function syspkgs
          );
    in
    # (system: function nixpkgs.legacyPackages.${system});
    {
      # packages.x86_64-linux.default = import ./shell.nix { inherit pkgs; };
      packages = forAllSystems (pkgs: {
        default = import ./shell.nix { inherit pkgs; };
      });
      nixosConfigurations =
        let
          # extraSpecialArgs = { inherit inputs; };
          mkHostConfig =
            { host, arch }:
            {
              name = host;
              value = lib.nixosSystem {
                system = arch;
                specialArgs = {
                  inherit inputs;
                };
                modules = [
                  ./hosts/${host}
                  {
                    nix.registry.nixpkgs.flake = inputs.nixpkgs; # nix shell to use system flake
                  }
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

          machineConfigs = autoMachineConfigs ++ [ ];
        in
        builtins.listToAttrs machineConfigs;

      homeManagerConfigurations = {
        sergio = home-manager.lib.homeManagerConfiguration {
          pkgs = import nixpkgs { system = "x86_64-linux"; };
          modules = [
            ./home/sergio/home.nix
            {
              home = {
                username = "sergio";
                homeDirectory = "/home/sergio";
                stateVersion = "23.11";
              };
            }
          ];
        };
      };
    };
}
