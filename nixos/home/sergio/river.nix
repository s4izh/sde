{
  config,
  pkgs,
  lib,
  sde,
  ...
}:
{
  wayland.windowManager.river = {
    enable = true;
    package = null;
    extraConfig = builtins.readFile "${sde.flakeRoot}/dotfiles/.config/river/init";
  };
}
