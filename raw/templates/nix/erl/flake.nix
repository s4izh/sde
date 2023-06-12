{
  description = "Erlang project";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        system = system;
        overlays = [
          (self: super: {
            erlang = super.erlangR24;
            rebar3 = super.rebar3.override {
              erlang = self.erlang;
            };
          })
        ];
      };
    in
    {
      devShell = pkgs.mkShell {
        buildInputs = [
          pkgs.erlang
          pkgs.rebar3
          pkgs.hex
        ];
        shellHook = ''
          export ERL_AFLAGS="-kernel shell_history enabled -kernel shell_history_size 1000"
          export ERL_LIBS="$(pwd)/_build/default/lib"
          export PATH="$PATH:${pkgs.rebar3}/bin:${pkgs.hex}/bin"
          rebar3 compile
        '';
      };
    }
  );
}
