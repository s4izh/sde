{
  inputs,
  config,
  pkgs,
  lib,
  sde,
  ...
}:
let
  vscode-insiders = (pkgs.vscode.override { isInsiders = true; }).overrideAttrs (oldAttrs: {
    src = (
      builtins.fetchTarball {
        url = "https://code.visualstudio.com/sha/download?build=insider&os=linux-x64";
        sha256 = "sha256:0vmndc40z0ahijjghpfb52kkkydcr107rbsg0hlan4yg5db1kl8n";
      }
    );
    version = "1.99-insider";
  });

  rustpkgs = with pkgs; [
    rustup
    cargo
    cargo-watch
    cargo-make
    # rust-analyzer
    rustfmt
    rustc
  ];
  latexpkgs = with pkgs; [
    texlive.combined.scheme-full
    texlab
    ltex-ls
  ];
  nixpkgs = with pkgs; [
    nixd
    nixfmt-rfc-style
    (writeShellScriptBin "nixpkgs-fmt" ''
      #!/usr/bin/env bash
      nixfmt $@
    '') # wrapper script since nixd calls nixpkgs-fmt
  ];
  luapkgs = with pkgs; [
    lua
    lua-language-server
  ];
  cpkgs = with pkgs; [
    gcc
    cmake
    clang
    clang-tools
    gdb
    universal-ctags
    ctags-lsp
  ];
  erlangpkgs = with pkgs; [
    rebar3
    erlang
    erlang-language-platform
  ];
  linuxpkgs = with pkgs; [
    man-pages
    mandoc
    man-db
  ];
  shpkgs = with pkgs; [
    nodePackages_latest.bash-language-server
    shellcheck
    shfmt
  ];
  gopkgs = with pkgs; [
    go
    gopls
  ];
  zigpkgs = with pkgs; [
    zig
    zls
  ];
  haskellpkgs = with pkgs; [ ghc ];
  gitpkgs = with pkgs; [
    git
    python312Packages.git-filter-repo
    gh # github cli
    glab # gitlab cli
  ];
  devpkgs = with pkgs; [
    tmux
    fzf
    bat
    gnumake
    direnv
    jq
    cloc
    plantuml
    wireshark
    yaml-language-server
  ];
  testpkgs = with sde.pkgs.${pkgs.system}; [
    test
  ];
  editorspkgs = with pkgs; [
    vim
    # neovim
    vscode
    inputs.neovim-nightly-overlay.packages.${pkgs.system}.default
    # vscode-insiders
  ];
in
{
  environment.systemPackages =
    devpkgs
    ++ gitpkgs
    ++ rustpkgs
    ++ latexpkgs
    ++ nixpkgs
    ++ luapkgs
    ++ cpkgs
    ++ erlangpkgs
    ++ linuxpkgs
    ++ shpkgs
    ++ gopkgs
    ++ zigpkgs
    ++ haskellpkgs
    ++ editorspkgs
    ++ testpkgs;
}
