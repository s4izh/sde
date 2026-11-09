{ config, pkgs, lib, ... }:

let
  cfg = config.sde.virtualization;
in
{
  options.sde.virtualization = {
    docker.enable = lib.mkEnableOption "Docker virtualization";
    podman.enable = lib.mkEnableOption "Podman containers";
    libvirt.enable = lib.mkEnableOption "libvirt virtualization";
    desktopTools = lib.mkEnableOption "Enable desktop tools";

    user = lib.mkOption {
      type = lib.types.str;
      default = "sergio";
      description = "User that should access virtualization groups";
    };
  };

  config = lib.mkMerge [

    (lib.mkIf cfg.docker.enable {
      virtualisation.docker.enable = true;

      environment.systemPackages = with pkgs; [
        docker-compose
        kompose
      ];

      users.users.${cfg.user}.extraGroups = [ "docker" ];
    })

    (lib.mkIf cfg.podman.enable {
      virtualisation.podman.enable = true;
      # virtualization.dockerCompat = true;

      environment.systemPackages = with pkgs; [
        podman-compose
      ];
    })

    (lib.mkIf cfg.libvirt.enable {
      virtualisation.libvirtd.enable = true;

      environment.systemPackages = with pkgs; [
        swtpm # tmp emulation module
      ];

      users.users.${cfg.user}.extraGroups = [ "libvirtd" ];
    })

    (lib.mkIf cfg.libvirt.enable {
      virtualisation.libvirtd.enable = true;

      programs.dconf.enable = true;

      # programs.dconf.settings = {
      #   "org/virt-manager/virt-manager/connections" = {
      #     autoconnect = [ "qemu:///system" ];
      #     uris = [ "qemu:///system" ];
      #   };
      # };

      environment.systemPackages = with pkgs; [
        virt-manager
        virt-viewer
        remmina
      ];
      users.users.${cfg.user}.extraGroups = [ "libvirtd" ];
    })
  ];
}
