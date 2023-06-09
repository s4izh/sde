{ pkgs ? import <nixpkgs> {} }:

let
  erlangVersion = "24.0";
  rebar3Version = "3.17.0";
  hexVersion = "0.21.0";
in

pkgs.mkShell {
  buildInputs = with pkgs; [
    erlangR24
    rebar3
    hex
  ];

  shellHook = ''
    # Set the Erlang runtime version
    export ERL_AFLAGS="-kernel shell_history enabled -kernel shell_history_size 1000"
    export ERL_LIBS="$(pwd)/_build/default/lib"

    # Set the PATH for rebar3 and hex
    export PATH="$PATH:${pkgs.rebar3}/bin:${pkgs.hex}/bin"

    # Automatically fetch and compile dependencies
    rebar3 compile
  '';
}
