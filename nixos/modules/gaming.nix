{
  config,
  pkgs,
  lib,
  ...
}:
{
  environment.systemPackages = with pkgs; [
    legendary-gl
    wineWowPackages.stable
    # dxvk
    heroic
    bottles

    mangohud
    gamescope
    oversteer

    obs-studio
    kdenlive
    ffmpeg
    lutris
    # webcord
    # minecraft
    prismlauncher
    # openjdk8-bootstrap
    jre8
    jdk8
    protonup-qt
    protontricks
    # xorg.libICE
    # xorg.libxcb
    # xorg.libXt
    # visualvm
    # haskellPackages.minecraft
    obs-studio
    # obs-cli
  ];

  # xdg.portal.enable = true;
  # xdg.portal.extraPortals = with pkgs; [ xdg-desktop-portal-gtk ];
  # services.flatpak.enable = true;

  # services.logmein-hamachi.enable = true;
  # programs.haguichi.enable = true;

  #Steam
  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
    dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
    gamescopeSession.enable = true;
  };

  programs.gamescope = {
    enable = true;
    capSysNice = true;
  };

  #G29 wheel
  hardware.new-lg4ff.enable = true;
  services.udev.packages = with pkgs; [ oversteer ];
}
