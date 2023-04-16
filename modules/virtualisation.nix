{ config, pkgs, lib, ... }:
{
  environment.systemPackages = with pkgs; [
    virt-manager
    virt-viewer
    docker
    docker-compose
    kompose
  ];

  virtualisation.docker.enable = true;

  programs.dconf.enable = true;
  virtualisation.libvirtd.enable = true;
  users.users.sergio.extraGroups = [ "libvirtd" "docker" ];
}
