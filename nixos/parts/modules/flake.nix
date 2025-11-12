{
  inputs,
  self,
  sde,
  ...
}:
{
  overlays.default = import "${sde.flakeRoot}/overlays" { inherit inputs; };
  nixosModules.sde = import "${sde.flakeRoot}/modules/sde/nixos";
  homeManagerModules.default = import "${sde.flakeRoot}/modules/home-manager";
  nixosConfigurations =
    let
      mkHostConfig =
        { host, arch }:
        {
          name = host;
          value = inputs.nixpkgs.lib.nixosSystem {
            system = arch;
            specialArgs = { inherit self inputs sde; };
            modules = [
              self.nixosModules.sde
              "${sde.nixosPrefix}/hosts/${host}"
              { nix.registry.nixpkgs.flake = inputs.nixpkgs; }
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
      ];
    in
    inputs.nixpkgs.lib.listToAttrs (map mkHostConfig hosts);

  homeManagerConfigurations = {
    sergio = inputs.home-manager.lib.homeManagerConfiguration {
      pkgs = import inputs.nixpkgs { system = "x86_64-linux"; };
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
}
