{
  config,
  lib,
  ...
}:
let
  cfg = config.sde.sshKeys;
  keys = import ../../../keys;
in
{
  options.sde.sshKeys = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = config.sde.base.enable;
      description = ''
        Authorize every user key in keys/users for the main user, and trust
        every host key in keys/hosts system-wide (also used by root, e.g. for
        sshfs mounts).
      '';
    };

    user = lib.mkOption {
      type = lib.types.str;
      default = "sergio";
      description = "User that gets the authorized keys.";
    };

    exclude = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      example = [ "proton" ];
      description = "Names of user keys (file names without .pub) not to authorize on this host.";
    };
  };

  config = lib.mkIf cfg.enable {
    users.users.${cfg.user}.openssh.authorizedKeys.keys = lib.attrValues (
      removeAttrs keys.users cfg.exclude
    );

    programs.ssh.knownHosts = lib.mapAttrs (host: key: {
      hostNames = [ host ];
      publicKey = key;
    }) (removeAttrs keys.hosts [ config.networking.hostName ]);
  };
}
