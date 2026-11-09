{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.sde.vpn;
in
{
  options.sde.vpn = {
    enable = lib.mkEnableOption "VPN services (Tailscale, Cloudflare Warp, OpenFortiVPN)";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      openfortivpn
    ];

    services.tailscale.enable = true;
    services.cloudflare-warp.enable = true;

    networking.firewall.checkReversePath = "loose";
  };
}
