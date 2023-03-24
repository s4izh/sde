{ config, pkgs, lib, ... }:
{
  environment.systemPackages = with pkgs; [
    virt-manager
  ];

  programs.dconf.enable = true;
  virtualisation.libvirtd.enable = true;
  users.users.sergio.extraGroups = [ "libvirtd" ];
}
