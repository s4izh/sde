{
  description = "Flake for my multisystem NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    hermes-agent = {
      url = "github:NousResearch/hermes-agent";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };
    nixos-hardware = {
      url = "github:nixos/nixos-hardware/master";
    };
    flake-utils.url = "github:numtide/flake-utils";
    nix-index-database = {
      url = "github:nix-community/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";
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
          function syspkgs syspkgs.stdenv.hostPlatform.system
        );

      sde = rec {
        flakeRoot = ../.;
        nixosPrefix = "${flakeRoot}/nix";
        scripts = "${flakeRoot}/dotfiles/.local/scripts";
        pkgs = forAllSystems (pkgs: system: import ./pkgs pkgs system) nixpkgs;

        # Private config: the nix/ folder of a local clone of s4izh/sde-private,
        # or null if there is none. Same layout as this folder, entry points
        # modules/sde/nixos/default.nix and home/sergio/home.nix. Reading a path
        # outside the flake needs --impure (the Makefile always passes it); in
        # pure eval (no builtins.currentSystem) it is skipped.
        private =
          let
            dir = "/home/sergio/personal/sde-private/nix";
          in
          if builtins ? currentSystem && builtins.pathExists dir then /. + dir else null;
      };

      sdeDocs = pkgs:
        let
          eval = lib.evalModules {
            modules = [
              { _module.check = false; }
              self.nixosModules.sde
            ];
          };
          # Strip the nix store path prefix to get a relative path like
          # "nix/modules/sde/nixos/base.nix", then build a GitHub URL.
          repoUrl = "https://github.com/s4izh/sde";
          transformDecl = decl:
            let
              rel = lib.removePrefix "${toString self}/" (toString decl);
              isLocal = rel != toString decl;
            in
            if isLocal then {
              url = "${repoUrl}/blob/main/nix/${rel}";
              name = "nix/${rel}";
            } else {
              url = toString decl;
              name = toString decl;
            };
        in
        pkgs.nixosOptionsDoc {
          options = eval.options.sde;
          transformOptions = opt: opt // {
            declarations = map transformDecl opt.declarations;
          };
        };

    in
    {
      packages = forAllSystems (pkgs: system:
        (import ./pkgs pkgs system) // {
          docs = (sdeDocs pkgs).optionsCommonMark;
          docs-json = (sdeDocs pkgs).optionsJSON;
        }
      ) nixpkgs;
      devShells = forAllSystems (pkgs: system: {
        default = import "${sde.nixosPrefix}/shell.nix" { inherit pkgs; } // {
          nativeBuildInputs = self.packages.${pkgs.stdenv.hostPlatform.system};
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

      homeManagerModules = {
        default = import ./modules/sde/home-manager;
        sde = import ./modules/sde/home-manager;
      };

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
                  # nix-index-database, enabled from sde.base
                  inputs.nix-index-database.nixosModules.nix-index
                ]
                ++ lib.optional (sde.private != null) (sde.private + "/modules/sde/nixos")
                ++ [
                  "${sde.nixosPrefix}/hosts/${host}"
                  {
                    nix.registry.nixpkgs.flake = inputs.nixpkgs; # nix shell to use system flake
                  }
                ];
              };
            };
          hosts = [
            {
              host = "rx";
              arch = "x86_64-linux";
            }
            {
              host = "zen";
              arch = "x86_64-linux";
            }
            {
              host = "dell";
              arch = "x86_64-linux";
            }
            {
              host = "jsc";
              arch = "x86_64-linux";
            }
          ];
          autoMachineConfigs = map mkHostConfig hosts;

          machineConfigs = autoMachineConfigs ++ [ ];
        in
        builtins.listToAttrs machineConfigs;

      hosts = builtins.attrNames self.nixosConfigurations;

      homeManagerConfigurations = {
        sergio = home-manager.lib.homeManagerConfiguration {
          pkgs = import nixpkgs {
            system = "x86_64-linux";
            config.allowUnfree = true;
          };
          # same args the hosts give home-manager through extraSpecialArgs
          extraSpecialArgs = { inherit inputs sde; };
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
