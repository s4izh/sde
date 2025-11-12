{
  self,
  config,
  pkgs,
  self',
  ...
}:
{
  packages = import "${config.sde.nixosPrefix}/pkgs" pkgs pkgs.system;
  devShells.default = import "${config.sde.nixosPrefix}/shell.nix" { inherit pkgs; } // {
    nativeBuildInputs = self'.packages;
  };
}
