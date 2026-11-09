# Acceso SSH al pc de javier: claves autorizadas para entrar y hacer rebuilds remotos.
{ ... }:

let
  sergioKey = (import ../../keys).users.proton;
  adminKeys = [ sergioKey ];
in
{
  # PC ajeno: solo las claves de abajo, no todas las de keys/users.
  sde.sshKeys.enable = false;

  services.openssh.enable = true;

  # Solo clave para root (antes estaba en "yes", que permite root por password).
  services.openssh.settings.PermitRootLogin = "prohibit-password";

  users.users.root.openssh.authorizedKeys.keys = adminKeys;
  users.users.javier.openssh.authorizedKeys.keys = adminKeys;
}
