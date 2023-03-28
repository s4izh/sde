{ pkgs, lib, inputs, ... }:
{
  environment.systemPackages = [
    pkgs.openssl
    pkgs.pkg-config
    pkgs.sqlx-cli
  ];

  services.mysql.enable = true;
  services.mysql.package = pkgs.mariadb;

  users.users.sergio.extraGroups = [ "mysql" ];
}
