{
  config,
  lib,
  pkgs,
  ...
}:
# santuario app (~/personal/santuario, docs/13-app.md): slurm.py polls BSC on
# timers and `sant serve` publishes what it writes (D20). Opt-in per host
# with sde.santuario.enable; the host publishes the server on the tailnet
# with sde.tailscaleServe (sde-private, hosts/rx).
let
  cfg = config.sde.santuario;
  home = config.home.homeDirectory;
  repo = "${home}/personal/santuario";
  # debug build: not compute bound, and `cargo build` alone redeploys it
  sant = "${repo}/target/debug/sant";
  slurm = "${repo}/tools/slurm/slurm.py";
  # ssh, python3 and notify-send
  path = lib.concatStringsSep ":" [
    "${config.home.profileDirectory}/bin"
    "/run/wrappers/bin"
    "/run/current-system/sw/bin"
  ];
  slurmJob = description: flag: {
    Unit.Description = description;
    Service = {
      Type = "oneshot";
      ExecStart = "${slurm} ${flag}";
      Environment = [ "PATH=${path}" ];
    };
  };
  every = description: interval: {
    Unit.Description = description;
    Timer = {
      OnStartupSec = "30s";
      OnUnitActiveSec = interval;
      # the default 1 min accuracy would bunch the polls up
      AccuracySec = "5s";
    };
    Install.WantedBy = [ "timers.target" ];
  };
in
{
  options.sde.santuario.enable = lib.mkEnableOption "santuario app server and the slurm.py timers";

  config = lib.mkIf cfg.enable {
    systemd.user.services.slurm-poll = slurmJob "Poll my Slurm queues on BSC" "--poll";
    systemd.user.timers.slurm-poll = every "Poll my Slurm queues every minute" "1min";

    # bsc_acct alone takes ~90 s, so reports never share the queue poll
    systemd.user.services.slurm-reports = slurmJob "Slurm history, quota and accounting" "--reports";
    systemd.user.timers.slurm-reports = every "Refresh Slurm reports" "15min";

    systemd.user.services.sant-serve = {
      Unit = {
        Description = "santuario app server (sant serve)";
        StartLimitIntervalSec = 120;
        StartLimitBurst = 5;
      };
      Service = {
        ExecStart = "${sant} serve --web-dir ${repo}/apps/web --slurm-cli ${slurm}";
        Restart = "on-failure";
        RestartSec = 5;
        Environment = [ "PATH=${path}" ];
      };
      Install.WantedBy = [ "default.target" ];
    };

    # restart the server whenever cargo leaves a new binary
    systemd.user.paths.sant-serve-reload = {
      Unit.Description = "Restart sant serve on a new build";
      Path.PathChanged = sant;
      Install.WantedBy = [ "paths.target" ];
    };
    systemd.user.services.sant-serve-reload = {
      Unit.Description = "Restart sant serve after a build";
      Service = {
        Type = "oneshot";
        ExecStart = "${pkgs.systemd}/bin/systemctl --user restart sant-serve.service";
      };
    };
  };
}
