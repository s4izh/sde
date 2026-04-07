{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.sde.latex;
  latexpkgs = with pkgs; [
    # texlive.combined.scheme-full
    texliveFull
    biber
    texlab
    ltex-ls
  ];
in
{
  options.sde.latex = {
    enable = lib.mkEnableOption "Enable latex";
  };

  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      environment.systemPackages = latexpkgs;
    })
  ];
}
