{ pkgs, lib, ... }:

pkgs.stdenv.mkDerivation {
  name = "test";
  src = null;

  phases = [ "installPhase" ];

  installPhase = ''
    mkdir -p $out/bin
    echo '#!/bin/sh' > $out/bin/my-hello
    echo 'echo "Hello from test package"' >> $out/bin/my-hello
    chmod +x $out/bin/my-hello
  '';

  meta = with lib; {
    description = "A test package that prints a message";
    license = licenses.mit;
    maintainers = with maintainers; [ s4izh ];
  };
}
