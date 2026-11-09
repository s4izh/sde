# Multica CLI + agent daemon (https://github.com/multica-ai/multica).
# Upstream release binary: statically linked Go, so no patching is needed.
# `multica update` can't work from the store; bump version + hashes here.
{
  lib,
  stdenvNoCC,
  fetchurl,
}:
let
  version = "0.8.0";
  sources = {
    x86_64-linux = {
      arch = "amd64";
      hash = "sha256-1q89ACQ0Tuj1HUxYrLq7iut+6n/RPlzLZaxn2PyE5Tw=";
    };
    aarch64-linux = {
      arch = "arm64";
      hash = "sha256-ZrWn0aHp0oY1gJcYf8jDfmEouboJBAfMS3gGKCL8WV4=";
    };
  };
  system = stdenvNoCC.hostPlatform.system;
  src = sources.${system} or (throw "multica: unsupported system ${system}");
in
stdenvNoCC.mkDerivation {
  pname = "multica";
  inherit version;

  src = fetchurl {
    url = "https://github.com/multica-ai/multica/releases/download/v${version}/multica-cli-${version}-linux-${src.arch}.tar.gz";
    inherit (src) hash;
  };
  sourceRoot = ".";

  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    runHook preInstall
    install -Dm755 multica $out/bin/multica
    runHook postInstall
  '';

  meta = {
    description = "Managed agents platform: CLI and local agent runtime daemon";
    homepage = "https://github.com/multica-ai/multica";
    # Apache-2.0 plus extra conditions ("Multica License"), not plain asl20
    license = lib.licenses.unfree;
    mainProgram = "multica";
    platforms = builtins.attrNames sources;
    sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
  };
}
