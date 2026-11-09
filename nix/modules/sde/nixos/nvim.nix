{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.sde.nvim;
in
{
  options.sde.nvim = {
    enable = lib.mkEnableOption "Neovim editor";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ neovim ];
  };
}
