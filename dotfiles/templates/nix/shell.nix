{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShell {
  name = "mini-scheme";
  # only build time time dependencies
  nativeBuildInputs = with pkgs; [
    python312Packages.antlr4-python3-runtime
    pyright
    antlr4
  ];
  # run and/or build time dependencies
  buildInputs = with pkgs; [
  ];
}
