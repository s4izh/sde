{
  config,
  pkgs,
  lib,
  ...
}:
{
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
    st
    sxhkd
    xorg.xwininfo
  ];

  nixpkgs.overlays = [
    (final: prev: {
      dwm = prev.dwm.overrideAttrs (old: {
        src = /home/sergio/.local/src/dwm;
        # src = fetchGit { url = "https://github.com/s4izh/dwm"; };
      });
      dmenu = prev.dmenu.overrideAttrs (old: {
        src = /home/sergio/.local/src/dmenu;
      });
      dwmblocks = prev.dwmblocks.overrideAttrs (old: {
        src = /home/sergio/.local/src/dwmblocks;
      });
      st = prev.st.overrideAttrs (old: {
        src = /home/sergio/.local/src/st;
        buildInputs = old.buildInputs ++ [ pkgs.harfbuzz ];
      });
    })
  ];

  # system.stateVersion = "22.11"; # Did you read the comment? yes
}
