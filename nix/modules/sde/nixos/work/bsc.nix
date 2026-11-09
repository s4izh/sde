{
  config,
  lib,
  pkgs,
  options,
  ...
}:
let
  cfg = config.sde.work.bsc;
in
{
  options.sde.work.bsc = {
    enable = lib.mkEnableOption "BSC work profile";
  };

  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      environment.systemPackages = with pkgs; [
        thunderbird
        openfortivpn
        # BSC-specific tooling (compilers, MPI, schedulers, ...)
        samba jre # printing
      ];

      # Example VPN service. Keep credentials out of the Nix store: use a
      # sops-nix/agenix secret path or a local file outside git.
      #
      # services.openvpn.servers.bsc = {
      #   configFile = config.sops.secrets."bsc/openvpn".path;
      #   autoStart = false;
      # };
    })

    # If the host uses home-manager, enable the matching user profile too.
    (lib.optionalAttrs (options ? home-manager) {
      home-manager.sharedModules = lib.mkIf cfg.enable [
        { sde.work.bsc.enable = true; }
      ];
    })
  ];
}
