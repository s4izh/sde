# Index of what this host serves, meant for the default `tailscale serve`
# port: the host publishes cfg.port with sde.tailscaleServe.routes.
# Modules register themselves in sde.dashboard.entries; each entry gets a card
# and a /<name> redirect to its own port on whatever hostname the request
# came in on, so the tailnet name never appears here.
{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.sde.dashboard;
  hostName = config.networking.hostName;
  entries = lib.attrValues (lib.mapAttrs (name: e: e // { inherit name; }) cfg.entries);
  scheme = e: if e.https then "https" else "http";

  card = e: ''
    <a class="card" href="/${e.name}">
      <span class="title">${lib.escapeXML e.title}</span>
      <span class="desc">${lib.escapeXML e.description}</span>
      <span class="port">${scheme e} :${toString e.port}</span>
    </a>
  '';

  site = pkgs.writeTextDir "index.html" ''
    <!doctype html>
    <html lang="en">
    <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <meta name="color-scheme" content="light dark">
    <title>${hostName}</title>
    <style>
      :root {
        --bg: #f6f5f4; --fg: #1e1e1e; --muted: #6b6b6b;
        --card: #ffffff; --line: #deddda; --accent: #3584e4;
      }
      @media (prefers-color-scheme: dark) {
        :root {
          --bg: #1e1e1e; --fg: #f0f0f0; --muted: #9a9996;
          --card: #2a2a2a; --line: #3d3d3d; --accent: #78aeed;
        }
      }
      * { box-sizing: border-box; }
      body {
        margin: 0; padding: 48px 16px; background: var(--bg); color: var(--fg);
        font: 16px/1.45 system-ui, sans-serif;
      }
      main { max-width: 760px; margin: 0 auto; }
      h1 { margin: 0 0 4px; font-size: 28px; font-weight: 650; letter-spacing: -0.01em; }
      p.sub { margin: 0 0 28px; color: var(--muted); }
      .grid { display: grid; gap: 12px; grid-template-columns: repeat(auto-fill, minmax(220px, 1fr)); }
      .card {
        display: flex; flex-direction: column; gap: 4px; padding: 16px;
        background: var(--card); border: 1px solid var(--line); border-radius: 10px;
        color: inherit; text-decoration: none;
      }
      .card:hover, .card:focus-visible { border-color: var(--accent); outline: none; }
      .title { font-weight: 600; }
      .desc { color: var(--muted); font-size: 14px; flex: 1; }
      .port { margin-top: 8px; color: var(--muted); font: 12px ui-monospace, monospace; }
    </style>
    </head>
    <body>
    <main>
      <h1>${hostName}</h1>
      <p class="sub">${toString (lib.length entries)} services on this host</p>
      <div class="grid">
    ${lib.concatMapStrings card entries}
      </div>
    </main>
    </body>
    </html>
  '';

  caddyfile = pkgs.writeText "dashboard-Caddyfile" ''
    {
      admin off
      auto_https off
    }

    http://:${toString cfg.port} {
      bind 127.0.0.1
      encode zstd gzip

      # temporary, so a moved port is never stuck in a browser cache
    ${lib.concatMapStrings (e: ''
      redir /${e.name} ${scheme e}://{host}:${toString e.port}/ 302
    '') entries}
      root * ${site}
      file_server
    }
  '';
in
{
  options.sde.dashboard = {
    enable = lib.mkEnableOption "index page linking to the services of this host";
    port = lib.mkOption {
      type = lib.types.port;
      default = 7640;
      description = "Loopback port of the page.";
    };
    entries = lib.mkOption {
      default = { };
      description = "Services to list. The attribute name is the /<name> redirect.";
      type = lib.types.attrsOf (
        lib.types.submodule {
          options = {
            title = lib.mkOption { type = lib.types.str; };
            description = lib.mkOption {
              type = lib.types.str;
              default = "";
            };
            port = lib.mkOption {
              type = lib.types.port;
              description = "Port the service is reachable on from the tailnet.";
            };
            https = lib.mkOption {
              type = lib.types.bool;
              default = true;
              description = "False for a service on plain http (not behind `tailscale serve`).";
            };
          };
        }
      );
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.sde-dashboard = {
      description = "Index of the services on this host (Caddy)";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      # caddy keeps a little state even with admin and auto_https off
      environment.XDG_DATA_HOME = "/var/lib/sde-dashboard";
      environment.XDG_CONFIG_HOME = "/var/lib/sde-dashboard";
      serviceConfig = {
        ExecStart = "${pkgs.caddy}/bin/caddy run --adapter caddyfile --config ${caddyfile}";
        Restart = "on-failure";
        RestartSec = 5;
        DynamicUser = true;
        StateDirectory = "sde-dashboard";
        ProtectHome = true;
        PrivateDevices = true;
        NoNewPrivileges = true;
      };
    };
  };
}
