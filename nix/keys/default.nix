# Public SSH keys for all my machines, one file per key.
#
#   users/<name>.pub  user keys (~/.ssh/id_ed25519.pub), allowed to log in
#   hosts/<host>.pub  host keys (/etc/ssh/ssh_host_ed25519_key.pub), trusted
#                     as known hosts under the name <host>
#
# Adding a machine: drop its two .pub files here and rebuild the others.
let
  inherit (builtins) attrNames readDir readFile filter match head;
  lib' = {
    trim = s: head (match "[[:space:]]*([^\n]*[^[:space:]])[[:space:]]*" s);
  };
  readKeys =
    dir:
    let
      files = filter (f: match ".*\\.pub" f != null) (attrNames (readDir dir));
    in
    builtins.listToAttrs (
      map (f: {
        name = head (match "(.*)\\.pub" f);
        value = lib'.trim (readFile (dir + "/${f}"));
      }) files
    );
in
{
  users = readKeys ./users;
  hosts = readKeys ./hosts;
}
