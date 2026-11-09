{
  config,
  lib,
  pkgs,
  options,
  ...
}:
let
  cfg = config.sde.work.bsc;

  # Connect to the BSC FortiGate VPN with split DNS: only *.bsc.es queries go
  # to the BSC nameservers over the tunnel, everything else (Tailscale
  # MagicDNS, LAN, public) keeps using the normal resolvers.
  #
  # openfortivpn's own DNS handling rewrites /etc/resolv.conf, which fights
  # with Tailscale and systemd-resolved, so it runs with --no-dns and the
  # tunnel interface is registered with resolved once pppd brings it up.
  bscVpn = pkgs.writeShellApplication {
    name = "bsc-vpn";
    runtimeInputs = with pkgs; [
      openfortivpn
      iproute2
      coreutils
      config.systemd.package
    ];
    text = ''
      if [[ $EUID -ne 0 ]]; then
        exec sudo --preserve-env=BSC_VPN_GATEWAY,BSC_VPN_USER "$0" "$@"
      fi

      readonly GATEWAY="''${BSC_VPN_GATEWAY:?not set, see sde-private}"
      readonly IFACE="bsc0"
      readonly DNS_SERVERS=(84.88.52.35 84.88.52.36)
      readonly DNS_DOMAINS=("~bsc.es")
      readonly POLL_SECONDS=1
      readonly vpn_user="''${BSC_VPN_USER:?not set, see sde-private}"

      configure_dns() {
        local parent=$1
        until [[ -n "$(ip -4 -o addr show dev "$IFACE" 2>/dev/null)" ]]; do
          kill -0 "$parent" 2>/dev/null || return 0
          sleep "$POLL_SECONDS"
        done
        resolvectl dns "$IFACE" "''${DNS_SERVERS[@]}"
        resolvectl domain "$IFACE" "''${DNS_DOMAINS[@]}"
        resolvectl default-route "$IFACE" false
        echo "bsc-vpn: split DNS for bsc.es configured on $IFACE"
      }

      configure_dns $$ &
      watcher=$!
      trap 'kill "$watcher" 2>/dev/null || true' EXIT

      openfortivpn "$GATEWAY" -u "$vpn_user" \
        --no-dns \
        --pppd-ifname="$IFACE" \
        "$@"
    '';
  };
in
{
  options.sde.work.bsc = {
    enable = lib.mkEnableOption "BSC work profile";
  };

  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      environment.systemPackages = with pkgs; [
        # openfortivpn itself comes from modules/vpn.nix; bsc-vpn wraps it
        # with split DNS for bsc.es.
        bscVpn

        thunderbird
        slack

        # BSC-specific tooling (compilers, MPI, schedulers, ...)
        samba jre # printing
      ];

      # Per-link DNS so the VPN tunnel can own bsc.es without replacing the
      # system resolver (and without Tailscale clobbering it).
      services.resolved.enable = true;
    })

    # If the host uses home-manager, enable the matching user profile too.
    (lib.optionalAttrs (options ? home-manager) {
      home-manager.sharedModules = lib.mkIf cfg.enable [
        { sde.work.bsc.enable = true; }
      ];
    })
  ];
}
