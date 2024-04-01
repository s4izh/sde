{
  pkgs,
  lib,
  inputs,
  ...
}: {
  environment.systemPackages = [
    pkgs.openssl
    pkgs.pkg-config
    pkgs.sqlx-cli
    # pkgs.libpqxx
    pkgs.postgresql
  ];

  services.mysql.enable = true;
  services.mysql.package = pkgs.mariadb;

  services.postgresql.enable = true;
  services.postgresql.port = 5432;

  users.users.sergio.extraGroups = ["postgres"];
}
