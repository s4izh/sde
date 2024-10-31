{
  config,
  pkgs,
  lib,
  ...
}:
{
  services.tailscale = {
    enable = true;
  };
  # allow VPN traffic routing
  networking.firewall.checkReversePath = "loose";
  # networking.firewall.allowedUDPPorts = [ 41641 ];

  # networking.networkmanager.vpnConnections = [
  #   {
  #     name = "tailscale";
  #     vpnType = "dummy"; # Use a dummy VPN type
  #   }
  # ];
}
