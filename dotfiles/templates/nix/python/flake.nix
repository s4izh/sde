{
  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/nixos-unstable";
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
  };
  outputs = { nixpkgs, flake-utils, ... }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
    in rec {
      devShell = pkgs.mkShell {
        buildInputs = with pkgs; [
          (python311.withPackages(ps: with ps; [
            streamlit
            watchdog          # For monitoring file changes
            python-frontmatter # For parsing YAML frontmatter
            websockets        # For the live-reload communication
            pandas            # Often useful with Streamlit
          ]))
          pyright
        ];
      };
    }
  );
}
