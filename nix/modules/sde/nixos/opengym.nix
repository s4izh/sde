# openGym (github.com/DuarteSantos8/openGym), self-hosted gym tracker.
# The upstream NixOS module (flake input opengym, contrib/nix) runs the API
# hardened as the opengym user; it leaves the web server to us, so a small
# Caddy puts the SPA and /api on one loopback origin for `tailscale serve`.
#
# Passkeys need HTTPS on one exact hostname. Both live per host in sde-private
# hosts/<host>: sde.opengym.host (the tailnet name stays out of here) and the
# sde.tailscaleServe route that publishes webPort on httpsPort.
# Changing the host or the port invalidates every registered passkey.
#
# Data (back this up): /var/lib/opengym
# Update: nix flake update opengym, then make switch
{
  config,
  pkgs,
  lib,
  inputs,
  ...
}:
let
  cfg = config.sde.opengym;

  # Upstream's api.nix (v1.4.5) installs only ./*.js and coach/, but server.js
  # also imports these folders (api/Dockerfile copies them) and dies with
  # ERR_MODULE_NOT_FOUND. Copies only what is missing, so it turns into a
  # no-op once upstream fixes it; drop it then.
  upstream = inputs.opengym.packages.${pkgs.stdenv.hostPlatform.system};
  packages = upstream // {
    opengym-api = upstream.opengym-api.overrideAttrs (old: {
      postInstall = (old.postInstall or "") + ''
        for d in library oidc integrations trainers sources; do
          [ -e $out/lib/opengym-api/$d ] || cp -r $d $out/lib/opengym-api/
        done
      '';
    });
  };

  caddyfile = pkgs.writeText "opengym-Caddyfile" ''
    {
      admin off
      auto_https off
    }

    http://:${toString cfg.webPort} {
      bind 127.0.0.1
      encode zstd gzip

      handle /api/* {
        reverse_proxy 127.0.0.1:${toString cfg.apiPort}
      }
      handle {
        root * ${config.services.opengym.web.root}
        try_files {path} /index.html
        file_server
      }
    }
  '';
in
{
  options.sde.opengym = {
    enable = lib.mkEnableOption "openGym (API and a loopback web server)";
    host = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      example = "myhost.tail1234.ts.net";
      description = ''
        Hostname in the browser's address bar, the passkey RP ID. Unset, the
        app only works on http://localhost:<webPort>.
      '';
    };
    httpsPort = lib.mkOption {
      type = lib.types.port;
      default = 8443;
      description = "HTTPS port it is published on in the tailnet.";
    };
    webPort = lib.mkOption {
      type = lib.types.port;
      default = 7650;
      description = "Loopback port of the web server.";
    };
    apiPort = lib.mkOption {
      type = lib.types.port;
      default = 7651;
      description = "Port of the API. It listens on every interface, keep it closed in the firewall.";
    };
  };

  config = lib.mkIf cfg.enable {
    services.opengym = {
      enable = true;
      package = packages;
      apiPort = cfg.apiPort;
      rpId = if cfg.host != null then cfg.host else "localhost";
      origin =
        if cfg.host == null then
          "http://localhost:${toString cfg.webPort}"
        else
          "https://${cfg.host}${lib.optionalString (cfg.httpsPort != 443) ":${toString cfg.httpsPort}"}";
    };

    sde.dashboard.entries.opengym = {
      title = "openGym";
      description = "Gym and body-weight tracker";
      port = cfg.httpsPort;
    };

    systemd.services.opengym-web = {
      description = "openGym web server (Caddy: SPA + /api)";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      # caddy keeps a little state even with admin and auto_https off
      environment.XDG_DATA_HOME = "/var/lib/opengym-web";
      environment.XDG_CONFIG_HOME = "/var/lib/opengym-web";
      serviceConfig = {
        ExecStart = "${pkgs.caddy}/bin/caddy run --adapter caddyfile --config ${caddyfile}";
        Restart = "on-failure";
        RestartSec = 5;
        DynamicUser = true;
        StateDirectory = "opengym-web";
        ProtectHome = true;
        PrivateDevices = true;
        NoNewPrivileges = true;
      };
    };
  };
}
