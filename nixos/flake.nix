{
  description = "Flake for my multisystem NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-hardware = {
      url = "github:nixos/nixos-hardware/master";
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
        let
          systems = [
            "x86_64-linux"
            "aarch64-linux"
          ];
        in
        function: pkgs:
        lib.genAttrs systems (
          system:
          let
            syspkgs = import pkgs {
              inherit system;
              config.allowUnfree = true;
            };
          in
          function syspkgs system
        );

      sde = rec {
        flakeRoot = ../.;
        nixosPrefix = "${flakeRoot}/nixos";
        scripts = "${flakeRoot}/dotfiles/.local/scripts";
        pkgs = forAllSystems (pkgs: system: import ./pkgs pkgs system) nixpkgs;
      };

    in
    {
      packages = sde.pkgs;
      devShells = forAllSystems (pkgs: system: {
        default = import "${sde.nixosPrefix}/shell.nix" { inherit pkgs; } // {
          nativeBuildInputs = self.packages.${pkgs.system};
        };
      }) nixpkgs;
      overlays = import ./overlays { inherit inputs; };
      # nixosModules = let
      #   moduleDefaults = [
      #     ./modules/nixos
      #     ./modules/sde/nixos
      #   ];

      # nixosModules.default = [ ];
      nixosModules.sde = import ./modules/sde/nixos;

      #   allModules = map (path: import path) moduleDefaults;
      #   in
      #     allModules;

      # nixosModules = import ./modules/sde/nixos;

      homeManagerModules = import ./modules/home-manager;

      nixosConfigurations =
        let
          mkHostConfig =
            { host, arch }:
            {
              name = host;
              value = lib.nixosSystem {
                system = arch;
                specialArgs = {
                  inherit self inputs sde;
                };
                modules = [
                  self.nixosModules.sde
                  "${sde.nixosPrefix}/hosts/${host}"
                  {
                    nix.registry.nixpkgs.flake = inputs.nixpkgs; # nix shell to use system flake
                  }
                ];
              };
            };
          hosts = [
            # {
            #   host = "z390";
            #   arch = "x86_64-linux";
            # }
            {
              host = "rx";
              arch = "x86_64-linux";
            }
            {
              host = "zen";
              arch = "x86_64-linux";
            }
            # {
            #   host = "jsc";
            #   arch = "x86_64-linux";
            # }
            # {
            #   host = "vm";
            #   arch = "x86_64-linux";
            # }
            # {
            #   host = "thinkcenter";
            #   arch = "x86_64-linux";
            # }
          ];
          autoMachineConfigs = map mkHostConfig hosts;

          machineConfigs = autoMachineConfigs ++ [ ];
        in
        builtins.listToAttrs machineConfigs;

      hosts = builtins.attrNames self.nixosConfigurations;

      homeManagerConfigurations = {
        sergio = home-manager.lib.homeManagerConfiguration {
          pkgs = import nixpkgs { system = "x86_64-linux"; };
          modules = [
            "${sde.nixosPrefix}/home/sergio/home.nix"
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
