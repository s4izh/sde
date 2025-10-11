{
  config,
  pkgs,
  lib,
  sde,
  ...
}:
let
  syncNotesScript = builtins.readFile "${sde.scripts}/sync-notes";

  syncNotes = pkgs.writeScriptBin "sync-notes" ''
    #!${pkgs.bash}/bin/bash
    export PATH=${pkgs.findutils}/bin:${pkgs.git}/bin:${pkgs.openssh}/bin:$PATH
    echo "running as $USER from $PWD"
    ${syncNotesScript}
  '';
in
{
  # home.packages = [ gitSyncNotesPkg ];

  # systemd.user.services.sync-notes = {
  #   Unit = {
  #     Description = "sync notes";
  #     Wants = "sync-notes.timer";
  #   };
  #   Service = {
  #     Environment = "PATH=${pkgs.coreutils}/bin:${pkgs.git}/bin:${pkgs.openssh}/bin:$PATH";
  #     ExecStart = "${syncNotes}/bin/sync-notes";
  #     Type = "oneshot";
  #   };
  # };

  # systemd.user.timers.sync-notes = {
  #   Unit.Description = "Run note syncing";
  #   Timer.OnCalendar = "*:0/1";
  #   Install.WantedBy = [ "timers.target" ];
  # };

}
