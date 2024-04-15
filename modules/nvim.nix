{
  config,
  pkgs,
  lib,
  ...
}: {
  # programs.neovim = {
  #   enable = true;
  #   package = pkgs.neovim;
  # };
  environment.systemPackages = with pkgs; [
    neovim
  ];
}
