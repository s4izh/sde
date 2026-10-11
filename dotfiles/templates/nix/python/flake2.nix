{
  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/nixos-unstable";
    };
  };
  outputs =
    { self, nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      pythonEnv = pkgs.python3.withPackages (ps: [
        ps.streamlit
        ps.pandas
        ps.numpy
      ]);
    in
    {
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          pythonEnv
          pyright
        ];
      };
    };
}
