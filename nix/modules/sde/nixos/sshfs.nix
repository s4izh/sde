{
  config,
  pkgs,
  lib,
  utils,
  ...
}:
let
  cfg = config.sde.sshfs;
  home = config.users.users.${cfg.user}.home;

  # Socket for "open this file here" requests coming from the remote hosts.
  # ssh forwards it (RemoteForward) to the same path on the remote, where
  # opout writes "<hostname>\t<path relative to $HOME>" to it; sde-open
  # (dotfiles/.local/scripts) opens it via ~/mnt. See nix/docs/ssh.md.
  openSock = "/run/user/${toString cfg.uid}/sde-open.sock";

in
{
  options.sde.sshfs = {
    user = lib.mkOption {
      type = lib.types.str;
      default = "sergio";
      description = "Local user that owns the mounts and whose SSH key is used.";
    };

    uid = lib.mkOption {
      type = lib.types.int;
      default = 1000;
      description = "Local uid the remote files are mapped to.";
    };

    gid = lib.mkOption {
      type = lib.types.int;
      default = 100;
      description = "Local gid the remote files are mapped to.";
    };

    mounts = lib.mkOption {
      default = { };
      description = ''
        Remote directories mounted on demand under ~/mnt/<name> via sshfs.
        They mount on first access and unmount after being idle.
      '';
      example = {
        rx.host = "rx";
      };
      type = lib.types.attrsOf (
        lib.types.submodule {
          options = {
            host = lib.mkOption {
              type = lib.types.str;
              description = "Remote host (e.g. Tailscale MagicDNS name).";
            };
            remoteUser = lib.mkOption {
              type = lib.types.str;
              default = cfg.user;
              description = "User to log in as on the remote host.";
            };
            path = lib.mkOption {
              type = lib.types.str;
              default = "/home/${cfg.user}";
              description = "Remote path to mount.";
            };
            idleTimeout = lib.mkOption {
              type = lib.types.str;
              default = "10min";
              description = "Unmount after this long without use.";
            };
          };
        }
      );
    };
  };

  config = lib.mkMerge [
    {
      # Let a new ssh session replace a stale forwarded socket (sde-open.sock)
      # left behind by a previous session.
      services.openssh.settings.StreamLocalBindUnlink = true;
    }
    (lib.mkIf (cfg.mounts != { }) {
      # Forward the open socket to every mounted host, so `<leader>O` (opout) in
      # nvim over ssh opens the file here through ~/mnt/<name>.
      programs.ssh.extraConfig = lib.concatMapStrings (m: ''
        Host ${m.host}
          RemoteForward ${openSock} ${openSock}
      '') (lib.attrValues cfg.mounts);

      systemd.user.sockets.sde-open = {
        description = "Open files from remote hosts via ~/mnt";
        wantedBy = [ "sockets.target" ];
        socketConfig = {
          ListenStream = "%t/sde-open.sock";
          SocketMode = "0600";
          Accept = true;
        };
      };

      systemd.user.services."sde-open@" = {
        description = "Open a file from a remote host via ~/mnt";
        serviceConfig = {
          ExecStart = "${home}/.local/scripts/sde-open";
          StandardInput = "socket";
          StandardOutput = "journal";
          # opout backgrounds the viewer; don't kill it when this unit exits.
          KillMode = "process";
        };
      };

      system.fsPackages = [ pkgs.sshfs ];

      # Needed so the user (not only root) can access a root-mounted FUSE fs.
      programs.fuse.userAllowOther = true;

      fileSystems = lib.mapAttrs' (
        name: m:
        lib.nameValuePair "${home}/mnt/${name}" {
          device = "${m.remoteUser}@${m.host}:${m.path}";
          fsType = "fuse.sshfs";
          options = [
            # Mount lazily on access, never block boot.
            "noauto"
            "nofail"
            "_netdev"
            "x-systemd.automount"
            "x-systemd.idle-timeout=${m.idleTimeout}"
            "x-systemd.mount-timeout=15s"
            "x-systemd.after=network-online.target"
            "x-systemd.after=tailscaled.service"

            # Files appear owned by the local user, and the kernel enforces normal
            # permission checks on them (without default_permissions, allow_other
            # would let any local user act as you on the remote).
            "allow_other"
            "default_permissions"
            "idmap=user"
            "uid=${toString cfg.uid}"
            "gid=${toString cfg.gid}"

            # The mount runs as root, so point ssh at the user's key. Host keys
            # come from the system-wide known_hosts (see ssh-keys.nix).
            "IdentityFile=${home}/.ssh/id_ed25519"
            "BatchMode=yes"

            # Survive flaky links instead of hanging forever.
            "reconnect"
            "ServerAliveInterval=15"
            "ServerAliveCountMax=3"
            "ConnectTimeout=10"
          ];
        }
      ) cfg.mounts;

      # By default, a few quick failed mounts (e.g. rx is off) hit systemd's start
      # limit and leave the automount dead until a manual reset-failed. Drop the
      # limit so every access just retries (each attempt gives up after ~10s).
      systemd.units = lib.mapAttrs' (
        name: _:
        lib.nameValuePair "${utils.escapeSystemdPath "${home}/mnt/${name}"}.mount" {
          overrideStrategy = "asDropin";
          text = ''
            [Unit]
            StartLimitIntervalSec=0
          '';
        }
      ) cfg.mounts;

      systemd.tmpfiles.rules = [
        "d ${home}/mnt 0755 ${cfg.user} users -"
      ]
      ++ lib.mapAttrsToList (name: _: "d ${home}/mnt/${name} 0755 ${cfg.user} users -") cfg.mounts;
    })
  ];
}
