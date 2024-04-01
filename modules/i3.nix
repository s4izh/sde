{
  config,
  pkgs,
  lib,
  ...
}: {
  services.xserver = {
    enable = true;
    displayManager.defaultSession = "none+i3";
    windowManager.i3.enable = true;
  };

  environment.systemPackages = with pkgs; [
    dmenu
    st
    sxhkd
    xorg.xwininfo
  ];

  nixpkgs.overlays = [
    (final: prev: {
      dmenu = prev.dmenu.overrideAttrs (old: {
        src = /home/sergio/.local/src/dmenu;
      });
      st = prev.st.overrideAttrs (old: {
        src = /home/sergio/.local/src/st;
        buildInputs = old.buildInputs ++ [pkgs.harfbuzz];
      });
    })
  ];

  # system.stateVersion = "22.11"; # Did you read the comment? yes
}
