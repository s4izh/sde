# beets: autotag + organize the music library fed by yt-dlp.
# Server side (navidrome, /srv/music, packages) lives in modules/music.nix.
# Notes: ~/notes/ai/music-selfhosted-navidrome.md
{ config, ... }:
{
  programs.beets = {
    enable = true;
    settings = {
      # Must match services.navidrome.settings.MusicFolder in modules/music.nix.
      directory = "/srv/music";
      library = "${config.xdg.dataHome}/beets/library.db";

      "import" = {
        move = true; # move out of the inbox instead of copying
        write = true; # write corrected tags into the files
        incremental = true; # skip dirs already imported
      };

      paths = {
        default = "$albumartist/$album%aunique{}/$track $title";
        singleton = "$artist/Non-Album/$title";
        comp = "Compilations/$album%aunique{}/$track $title";
      };

      # All of these ship enabled in nixpkgs' beets, deps included.
      plugins = [
        "chroma" # acoustic fingerprinting - helps, but won't save a bad rip
        "fromfilename" # derive artist/title from "Artist - Title.opus" when tags are junk
        "edit" # `beet edit` - bulk-fix metadata in $EDITOR, the YouTube-rip lifesaver
        "fetchart"
        "embedart"
        "lastgenre"
        "duplicates"
        "scrub" # drop the junk tags yt-dlp/YouTube leave behind
        "info"
        "smartplaylist" # generate .m3u from beets queries, picked up by Navidrome
      ];

      smartplaylist = {
        # Inside MusicFolder so Navidrome sees it; see PlaylistsPath in modules/music.nix.
        playlist_dir = "/srv/music/playlists";
        auto = true; # regenerate after every import/modify
        # Absolute paths in the .m3u (no relative_to) - always resolvable by Navidrome.
        playlists = [
          {
            name = "Todo.m3u";
            query = ""; # empty query = every item in the library
          }
          {
            # Hand-curated via a beets flexible attribute:
            #   beet modify -y gym=1 <query>     add
            #   beet modify -y gym! <query>      remove the field
            name = "Gym.m3u";
            query = "gym:1";
          }
          {
            name = "Recientes.m3u";
            query = "added:-1w..";
          }
          # Genre-based playlists are pointless until `beet lastgenre` has been
          # run over the library - right now only 1 of 158 items has a genre.
          # { name = "HipHop.m3u"; query = "genre:hip-hop"; }
        ];
      };

      chroma.auto = true;
      fetchart.auto = true;
      embedart.auto = true;
      lastgenre.auto = true;

      # Keep filenames portable across the phone/SD cards.
      asciify_paths = true;
    };
  };
}
