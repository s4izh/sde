{ pkgs, ... }:

pkgs.stdenv.mkDerivation {
  name = "test";
  src = null;

  phases = [ "installPhase" ];

  installPhase = ''
    mkdir -p $out/bin
    echo '#!/bin/sh' > $out/bin/hello
    echo 'echo "Hello from test package"' >> $out/bin/hello
    chmod +x $out/bin/hello
  '';

  meta = with pkgs.stdenv.lib; {
    description = "A test package that prints a message";
    license = licenses.mit;
    maintainers = with maintainers; [ s4izh ];
  };
}
