{ config, pkgs, lib, ... }: {
  services.xserver = {
    enable = true;
    windowManager.exwm.enable = true;
    displayManager.lightdm.enable = true;
  };

  environment.systemPackages = with pkgs; [
  ];

  # system.stateVersion = "22.11"; # Did you read the comment? yes
}
