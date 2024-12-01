{
  config,
  pkgs,
  lib,
  sde,
  ...
}:
let
  rustpkgs = with pkgs; [
    rustup
    cargo
    cargo-watch
    cargo-make
    rust-analyzer
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
  luapkgs = with pkgs; [ lua-language-server ];
  cpkgs = with pkgs; [
    gcc
    cmake
    clang
    clang-tools
    gdb
    universal-ctags
  ];
  erlangpkgs = with pkgs; [
    rebar3
    erlang
    erlang-ls
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
  ];
  devpkgs = with pkgs; [
    tmux
    fzf
    bat
    gnumake
    direnv
    jq
    pstree
    cloc
  ];
  testpkgs = [
    sde.pkgs.test
  ];
  editorspkgs = with pkgs; [
    vim
    neovim
    vscode
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
    ++ editorspkgs;
    # ++ testpkgs;
}
