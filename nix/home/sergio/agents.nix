# AI agent stack: Multica daemon + Hermes (ACP) on the Claude subscription.
# Design and rationale: docs/agents.md. Opt-in per host with sde.agents.enable.
{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  cfg = config.sde.agents;
  home = config.home.homeDirectory;
  system = pkgs.stdenv.hostPlatform.system;

  # The default Hermes build lacks the ACP extra and lazy installs can't
  # write to /nix/store, so bake the group in.
  hermes = inputs.hermes-agent.packages.${system}.default.override {
    extraDependencyGroups = [ "acp" ];
  };

  # Same commit the Hermes plugin catalog pins (claude-subscription-directsdk v0.3.4).
  officialPlugins = pkgs.fetchFromGitHub {
    owner = "NousResearch";
    repo = "hermes-official-plugins";
    rev = "83f0a896b3518c7131181838e340c9a80c79524d";
    hash = "sha256-KIJAohFLo4rqhziiisLQTmXsXYGnyvIb/kW+OEHVD/g=";
  };
  directsdkPlugin = pkgs.runCommand "claude-subscription-directsdk" { } ''
    cp -r ${officialPlugins}/claude-subscription-directsdk $out
  '';

  # Multica runs each Hermes task in a HERMES_HOME overlay under
  # ~/.multica/hermes-sessions that mirrors ~/.hermes/.managed (HM's marker).
  # A managed Hermes never creates its home skeleton, so the overlay fails with
  # "Required directory does not exist: .../cron". Create it, then exec.
  # The list mirrors _HERMES_HOME_SUBDIRS in hermes_cli/config.py.
  hermesForMultica = pkgs.writeShellScriptBin "hermes" ''
    if [ -n "''${HERMES_HOME:-}" ]; then
      for d in cron sessions logs logs/curator memories pairing hooks image_cache audio_cache skills; do
        [ -e "$HERMES_HOME/$d" ] || mkdir -p -m 700 "$HERMES_HOME/$d"
      done
    fi
    exec ${lib.getExe' hermes "hermes"} "$@"
  '';

  claudeConfigDir = "${home}/.claude-personal";
  claudeBin = "${home}/.npm/bin/claude"; # npm i -g @anthropic-ai/claude-code
  claudeWrapper = "${home}/.local/scripts/claude"; # sets CLAUDE_CONFIG_DIR
  multica = pkgs.callPackage ../../pkgs/multica.nix { };
in
{
  imports = [ inputs.hermes-agent.homeManagerModules.default ];

  options.sde.agents = {
    enable = lib.mkEnableOption "Hermes Agent (ACP) and the Multica daemon";
    multica.enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Run the Multica agent daemon as a systemd user service.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = lib.optional cfg.multica.enable multica;

    programs.hermes-agent = {
      enable = true;
      package = hermes;
    };

    # Used for its activation (config.yaml / .env / plugins). No long-running
    # Hermes service: Multica and editors spawn `hermes acp` per session.
    services.hermes-agent = {
      enable = true;
      package = hermes;
      gateway.enable = false;
      extraPlugins = [ directsdkPlugin ];
      # Home Manager marks config.yaml as managed, so `hermes model` can't save:
      # the model is chosen here. `hermes model` still lists what the account offers.
      settings = {
        model = {
          provider = "claude-subscription-directsdk-experimental";
          default = "claude-sonnet-5-5[1m]";
        };
        approvals.timeout = 300;
        terminal.backend = "local";
        plugins.entries.claude-subscription-directsdk-experimental.settings = {
          claude_config_dir = claudeConfigDir;
        };
      };
      # No ANTHROPIC_API_KEY here: the DirectSDK plugin refuses inherited keys.
      environment.CLAUDE_SUBSCRIPTION_DIRECTSDK_COMMAND = claudeBin;
    };

    systemd.user.services.multica-daemon = lib.mkIf cfg.multica.enable {
      Unit = {
        Description = "Multica agent runtime daemon";
        After = [ "network-online.target" ];
        Wants = [ "network-online.target" ];
        StartLimitIntervalSec = 120;
        StartLimitBurst = 5;
      };
      Service = {
        Type = "simple";
        ExecStart = "${lib.getExe multica} daemon start --foreground";
        Environment = [
          "PATH=${
            lib.concatStringsSep ":" [
              "${home}/.local/scripts"
              "${home}/.npm/bin"
              "${config.home.profileDirectory}/bin"
              (lib.makeBinPath (
                with pkgs;
                [
                  git
                  gh
                  nodejs
                  bash
                  coreutils
                ]
              ))
              "/run/wrappers/bin"
              "/run/current-system/sw/bin"
            ]
          }"
          "MULTICA_CLAUDE_PATH=${claudeWrapper}"
          "MULTICA_HERMES_PATH=${lib.getExe hermesForMultica}"
          "HERMES_HOME=${config.services.hermes-agent.hermesHome}"
        ];
        Restart = "on-failure";
        RestartSec = 10;
      };
      Install.WantedBy = [ "default.target" ];
    };
  };
}
