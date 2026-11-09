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

    xdg.configFile."shell/rc.d/bsc.bash".text = ''
      export BSC_ENV=1
      export PATH="$HOME/.opencode/bin:$PATH"
      # export PATH="$HOME/work/bin:$PATH"
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
