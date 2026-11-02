{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.sde.gaming;
in
{
  options.sde.gaming = {
    enable = lib.mkEnableOption "Gaming setup (Steam, Heroic, Lutris, MangoHud)";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      legendary-gl
      heroic
      bottles
      mangohud
      gamescope
      oversteer
      obs-studio
      kdePackages.kdenlive
      ffmpeg
      lutris
      prismlauncher
      jre8
      jdk8
      protonup-qt
      protontricks
    ];

    programs.steam = {
      enable = true;
      remotePlay.openFirewall = true;
    };
  };
}
