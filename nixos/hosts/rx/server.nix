{
  inputs,
  config,
  pkgs,
  sde,
  ...
}:
{
  networking.firewall = {
    enable = true;
    allowedTCPPorts = [
      3000
      443
    ];
  };

  services.avahi = {
    enable = true;
    nssmdns4 = true;
    settings.publish = {
      disable-publishing = false;
      publish-addresses = true;
      publish-workstation = true;
    };
  };

  services.caddy = {
    enable = true;
    virtualHosts."rx.local" = {
      extraConfig = ''
        tls internal
        reverse_proxy localhost:3000
      '';
    };
  };
}
