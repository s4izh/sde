{
  imports = [
    ./desktop.nix
    ./virtualisation.nix
    ./latex.nix
    ./kde.nix
    ./work/bsc.nix
  ];
  # sde.services.desktop = import ./desktop.nix;
}
