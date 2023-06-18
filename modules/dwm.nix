{ config, pkgs, lib, ... }: {
  services.xserver = {
    enable = true;
    windowManager.dwm.enable = true;
    displayManager.startx.enable = true;
    # displayManager.session = [
    #   {
    #   manage = "desktop";
    #   name = "dwm";
    #   start = ''exec startx'';
    #   }
    # ];
    # displayManager.defaultSession = "dwm";
    # desktopManager.xterm.enable = false;
  };

  environment.systemPackages = with pkgs; [
    dwm
    dwmblocks
    dmenu
    sxhkd
    xorg.xwininfo
    # st
    # harfbuzz
  ];


  nixpkgs.overlays = [
    (final: prev: {
      dwm = prev.dwm.overrideAttrs (old: {
        src = /home/sergio/.local/src/dwm;
        # src = fetchGit { url = "https://github.com/s4izh/dwm"; };
      });
      dmenu = prev.dmenu.overrideAttrs (old: {
        src = /home/sergio/.local/src/dmenu ;
        # src = fetchGit { url = "https://github.com/s4izh/dmenu"; };
      });
      dwmblocks = prev.dwmblocks.overrideAttrs (old: {
        src = /home/sergio/.local/src/dwmblocks ;
        # src = fetchGit { url = "https://github.com/s4izh/dwmblocks"; };
      });
    })
  ];

  # system.stateVersion = "22.11"; # Did you read the comment? yes
}
