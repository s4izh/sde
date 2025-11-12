{ lib, self, ... }:

{
  options.sde = lib.mkOption {
    type = lib.types.attrs;
    description = "Common paths and settings for the flake.";
    readOnly = true;
  };

  config.sde = rec {
    flakeRoot = self/../..;
    nixosPrefix = "${flakeRoot}/nixos";
    scripts = "${flakeRoot}/dotfiles/.local/scripts";
  };
}
