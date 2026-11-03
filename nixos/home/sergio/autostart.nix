{
  config,
  lib,
  ...
}:
let
  cfg = config.sde.autostart;

  # render one command per line
  render =
    kind: lines:
    lib.concatStringsSep "\n" ([ "# managed by home-manager: sde.autostart.${kind}" ] ++ lines) + "\n";
in
{
  options.sde.autostart = {
    add = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      example = [ "spotify" ];
      description = ''
        Commands to start on login in addition to the ones in mango's
        autostart.scm. Written to ~/.config/sde/autostart.add.
      '';
    };

    remove = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      example = [ "discord" ];
      description = ''
        Commands from mango's autostart.scm that must not start on this host.
        Written to ~/.config/sde/autostart.remove. This also cancels the same
        command if it was added with `add`.
      '';
    };
  };

  config = {
    xdg.configFile."sde/autostart.add".text = render "add" cfg.add;
    xdg.configFile."sde/autostart.remove".text = render "remove" cfg.remove;
  };
}
