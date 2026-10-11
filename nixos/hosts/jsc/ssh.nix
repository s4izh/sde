# Acceso SSH al pc de javier: claves autorizadas para entrar y hacer rebuilds remotos.
{ ... }:

let
  sergioKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIELMIuRYnGVnPQjIjWyMkrswEQGsbOXi2gf1OMNczCfk sergio.sanz.martinez@proton.me";
  adminKeys = [ sergioKey ];
in
{
  services.openssh.enable = true;

  # Solo clave para root (antes estaba en "yes", que permite root por password).
  services.openssh.settings.PermitRootLogin = "prohibit-password";

  users.users.root.openssh.authorizedKeys.keys = adminKeys;
  users.users.javier.openssh.authorizedKeys.keys = adminKeys;
}
