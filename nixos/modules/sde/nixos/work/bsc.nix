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
        # VPN / remote access tools go here. Some are already provided by
        # modules/vpn.nix (openfortivpn, tailscale, cloudflare-warp).
        # openconnect
        # openvpn
        # sshfs
        # remmina

        # BSC-specific tooling (compilers, MPI, schedulers, ...)
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
