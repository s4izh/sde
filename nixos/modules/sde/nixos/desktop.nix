{
  config,
  options,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.sde.desktop;
in
{
  imports = [ ];

  options = {
    sde.desktop.enable = lib.mkEnableOption "Enable SDE desktop";

    sde.desktop.fullEnvironment = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        Whether a full desktop environment (Plasma, GNOME) is in use. Those
        manage GTK theme and cursor themselves, so the home-manager theming
        (sde.theming in home-manager) gets disabled to avoid fighting them.
      '';
    };
  };

  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      environment.systemPackages = with pkgs; [
        guile
      ];
    })

    # only when the host uses home-manager as a NixOS module
    (lib.optionalAttrs (options ? home-manager) {
      home-manager.sharedModules = [
        { sde.theming.enable = lib.mkDefault (!cfg.fullEnvironment); }
      ];
    })
  ];
}
