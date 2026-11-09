# Self-hosted music library: yt-dlp -> beets -> /srv/music -> Navidrome.
{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.sde.music;
  musicDir = "/srv/music";
  inboxDir = "/srv/music-inbox";
  navidromePort = 4533;
in
{
  options.sde.music = {
    enable = lib.mkEnableOption "Self-hosted Navidrome music server";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      yt-dlp
      ffmpeg
    ];

    services.navidrome = {
      enable = true;
      openFirewall = true;
      settings = {
        Address = "0.0.0.0";
        Port = navidromePort;
        MusicFolder = musicDir;
        PlaylistsPath = "playlists";
        Scanner.Schedule = "@every 1h";
      };
    };

    systemd.tmpfiles.rules = [
      "d ${musicDir} 2775 sergio navidrome - -"
      "d ${musicDir}/playlists 2775 sergio navidrome - -"
      "d ${inboxDir} 2775 sergio sergio - -"
    ];
  };
}
