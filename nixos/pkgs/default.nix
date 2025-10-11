# build these directly using 'nix build .#example'
pkgs: system: {
  test = pkgs.callPackage ./test.nix { };
  ctags-lsp = pkgs.callPackage ./ctags-lsp.nix { };
  zen-browser = pkgs.callPackage ./zen.nix {
    fetchTarball = builtins.fetchTarball;
    desktopFile = ./zen.desktop;
  };
}
