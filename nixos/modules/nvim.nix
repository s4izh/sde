{
  config,
  pkgs,
  lib,
  ...
}:
# Treesitter parsers are installed by nvim-treesitter itself (see
# lua/sergio/treesitter.lua), which keeps the config identical across NixOS and
# non-Nix machines.
#
# To manage them with Nix instead, uncomment the block below. treesitter.lua
# looks for parser/<lang>.so on the runtimepath, so it will use these and only
# compile whatever is still missing -- no config change needed.
#
# Notes if you ever enable it:
#   * Do NOT use pkgs.neovim.override { configure = ...; }. That wrapper sets
#     VIMINIT to a generated init.lua, which takes precedence over
#     ~/.config/nvim/init.lua and silently disables the whole user config.
#   * share/* is not linked into the system profile by default, hence
#     environment.pathsToLink.
#   * nixpkgs also ships an `org` grammar, which collides with the one the
#     orgmode plugin bundles ("Multiple org parsers found"); filter it out.
#
# let
#   toLang = n: lib.replaceStrings [ "-" ] [ "_" ] (lib.removePrefix "tree-sitter-" n);
#
#   excluded = [ "org" ];
#
#   grammars = lib.filterAttrs (n: _: !(lib.elem (toLang n) excluded))
#     pkgs.tree-sitter.builtGrammars;
#
#   treesitterParsers = pkgs.runCommand "nvim-treesitter-parsers" { } (''
#     mkdir -p $out/share/nvim/site/parser
#   '' + lib.concatStringsSep "\n" (lib.mapAttrsToList
#     (n: g: "ln -s ${g}/parser $out/share/nvim/site/parser/${toLang n}.so")
#     grammars));
# in
{
  environment.systemPackages = with pkgs; [ neovim ];

  # environment.systemPackages = with pkgs; [ neovim treesitterParsers ];
  # environment.pathsToLink = [ "/share/nvim" ];
}
