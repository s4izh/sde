# Declarative `tailscale serve`: every route in sde.tailscaleServe.routes is
# published as https://<host>.<tailnet>.ts.net:<httpsPort> (tailnet only) and
# proxied to a loopback port. The serve config lives in tailscaled's state, so
# a oneshot rewrites it on boot and on every switch that changes the routes.
# It resets first: routes added by hand with `tailscale serve` do not survive.
{
  config,
  pkgs,
  lib,
  ...
}:
let
  routes = lib.attrValues config.sde.tailscaleServe.routes;
  httpsPorts = map (r: r.httpsPort) routes;
  tailscale = lib.getExe config.services.tailscale.package;
in
{
  options.sde.tailscaleServe.routes = lib.mkOption {
    default = { };
    description = "Loopback services to publish on the tailnet with `tailscale serve`.";
    example = {
      myapp = {
        httpsPort = 8443;
        localPort = 8080;
      };
    };
    type = lib.types.attrsOf (
      lib.types.submodule {
        options = {
          httpsPort = lib.mkOption {
            type = lib.types.port;
            default = 443;
            description = "HTTPS port on the tailnet address.";
          };
          localPort = lib.mkOption {
            type = lib.types.port;
            description = "Port on 127.0.0.1 it proxies to (plain http).";
          };
        };
      }
    );
  };

  config = lib.mkIf (routes != [ ]) {
    assertions = [
      {
        assertion = config.services.tailscale.enable;
        message = "sde.tailscaleServe.routes needs services.tailscale (sde.vpn.enable).";
      }
      {
        assertion = lib.allUnique httpsPorts;
        message = "sde.tailscaleServe.routes: two routes share an httpsPort.";
      }
    ];

    systemd.services.tailscale-serve = {
      description = "Publish sde.tailscaleServe.routes with tailscale serve";
      after = [ "tailscaled.service" ];
      wants = [ "tailscaled.service" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        # tailscaled may still be logging in at boot, or offline
        Restart = "on-failure";
        RestartSec = 15;
      };
      script = ''
        # serve needs a logged-in, running node
        for _ in $(seq 30); do
          ${tailscale} status --peers=false >/dev/null 2>&1 && break
          sleep 2
        done
        ${tailscale} serve reset
        ${lib.concatMapStrings (r: ''
          ${tailscale} serve --bg --https=${toString r.httpsPort} http://127.0.0.1:${toString r.localPort}
        '') routes}
      '';
    };
  };
}
