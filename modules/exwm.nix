{
  config,
  pkgs,
  lib,
  ...
}: {
  services.xserver = {
    enable = true;
    # windowManager.exwm.enable = true;
    displayManager.lightdm.enable = true;
    displayManager.session = [
      {
        manage = "desktop";
        name = "exwm";
        start = ''sh /home/sergio/.config/emacs/exwm-start.sh'';
      }
    ];
  };

  environment.systemPackages = with pkgs; [
  ];

  # system.stateVersion = "22.11"; # Did you read the comment? yes
}
