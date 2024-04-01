{
  config,
  pkgs,
  lib,
  ...
}: {
  services.xserver = {
    enable = true;
    desktopManager = {
      xterm.enable = false;
      xfce = {
        enable = true;
        noDesktop = true;
        enableXfwm = false;
      };
    };
    displayManager.defaultSession = "xfce";
    windowManager.i3.enable = true;
  };

  environment.systemPackages = with pkgs; [
    dmenu
    # sxhkd
    xorg.xwininfo
  ];

  nixpkgs.overlays = [
    (final: prev: {
      dmenu = prev.dmenu.overrideAttrs (old: {
        src = /home/sergio/.local/src/dmenu;
      });
    })
  ];
}
