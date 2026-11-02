{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.sde.work.bsc;
in
{
  options.sde.work.bsc = {
    enable = lib.mkEnableOption "BSC work profile (user)";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      # User-level BSC tools go here.
    ];

    # Work git identity. dotfiles/.config/git/config includes this file for
    # repositories under ~/work/:
    #
    #   [includeIf "gitdir:~/work/"]
    #       path = ~/.config/work/git.inc
    xdg.configFile."work/git.inc".text = ''
      [user]
          name = "Sergio Sanz"
          email = "sergio.sanz.martinez@bsc.es"
    '';

    # SSH entries for BSC hosts. Each machine keeps its own private key; only
    # the public key is uploaded to the BSC GitLab/cluster.
    programs.ssh.matchBlocks = {
      # "bsc-git" = {
      #   hostname = "gitlab.bsc.es";
      #   user = "sergio.sanz.martinez";
      #   identityFile = "~/.ssh/id_ed25519_bsc";
      #   identitiesOnly = true;
      # };
    };
  };
}
