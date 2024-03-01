{ config, pkgs, lib, ... }:
{
  # environment.systemPackages = with pkgs; [
  #     neovim
  # ];

  # nixpkgs.overlays = [
  #   (final: prev: {
  #     neovim = prev.neovim.overrideAttrs (old: {
  #         src = prev.fetchFromGitHub {
  #         owner = "neovim";
  #         repo = "neovim";
  #         rev = "master";
  #         # If you don't know the hash, the first time, set:
  #         # hash = "";
  #         # then nix will fail the build with such an error message:
  #         # hash mismatch in fixed-output derivation '/nix/store/m1ga09c0z1a6n7rj8ky3s31dpgalsn0n-source':
  #         hash = "sha256-dCwN7Z4t+pmGuH90Dff5h1qIm2Rh917cZX3GF/W5GYk=";
  #         };
  #     });
  #   })
  # ];

  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
             url = https://github.com/nix-community/neovim-nightly-overlay/archive/master.tar.gz;
             }))
  ];

  programs.neovim = {
    enable = true;
    package = pkgs.neovim-nightly;
  };
}

