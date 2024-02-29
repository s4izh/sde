{ config, pkgs, lib, ... }:
{
  environment.systemPackages = with pkgs; [
    nixd
    alejandra
    clang
    lua-language-server
    texlab
    ltex-ls # languagetool
  ];
}
