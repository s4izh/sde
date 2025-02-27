{
  config,
  pkgs,
  lib,
  ...
}:
{
  environment.systemPackages = with pkgs; [
    virt-manager
    virt-viewer
    remmina
    swtpm
    docker
    docker-compose
    kompose
    # win-virtio # TODO: make this optional as module
  ];

  virtualisation.docker.enable = true;

  programs.dconf.enable = true;
  programs.dconf.settings = {
    "org/virt-manager/virt-manager/connections" = {
      autoconnect = [ "qemu:///system" ];
      uris = [ "qemu:///system" ];
    };
  };
  virtualisation.libvirtd.enable = true;
  users.users.sergio.extraGroups = [
    "libvirtd"
    "docker"
  ];
}
