# default.nix
{
  lib,
  buildGoModule,
  fetchFromGitHub,
  go,
}:

buildGoModule rec {
  pname = "ctags-lsp";
  version = "v1.0.0";

  src = fetchFromGitHub {
    owner = "netmute";
    repo = "ctags-lsp";
    rev = "454d9e6c5ac5d5c9ac625eb5ab01ff1670439f37";
    sha256 = "...";
  };

  vendorSha256 = lib.optionalString (src != null) "";

  meta = with lib; {
    description = "A Language Server Protocol (LSP) implementation using `universal-ctags` for code completion and go-to definition.";
    license = licenses.mit;
    platforms = platforms.all;
  };
}
