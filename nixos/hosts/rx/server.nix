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
    publish = {
      enable = true;
      addresses = true;
      workstation = true;
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
