# # You can build these directly using 'nix build .#example'
# {
#   pkgs ?
#     let
#       # If pkgs is not defined, instantiate nixpkgs from locked commit
#       lock = (builtins.fromJSON (builtins.readFile ./flake.lock)).nodes.nixpkgs.locked;
#       nixpkgs = fetchTarball {
#         url = "https://github.com/nixos/nixpkgs/archive/${lock.rev}.tar.gz";
#         sha256 = lock.narHash;
#       };
#       system = builtins.currentSystem;
#       overlays = [ ]; # Explicit blank overlay to avoid interference
#     in
#     import nixpkgs { inherit system overlays; },
#   ...
# }:
pkgs: system: {
  test = pkgs.callPackage ./test.nix { };
  ctags-lsp = pkgs.callPackage ./ctags-lsp.nix { };
  zen-browser = pkgs.callPackage ./zen.nix { 
    fetchTarball = builtins.fetchTarball;
    desktopFile = ./zen.desktop; };
}
