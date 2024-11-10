{
  description = "Flake for my multisystem NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
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
    {
      devShells = forAllSystems (pkgs: {
        default = import "shell.nix" { inherit pkgs; };
      });
      nixosConfigurations = {
        casa = lib.nixosSystem {
          modules = [ ./configuration.nix ];
          specialArgs = {
            inherit inputs;
          };
        };
      };
    };
}
