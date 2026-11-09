{
  config,
  pkgs,
  lib,
  ...
}:

{
  networking.hostName = "dell";
  networking.networkmanager.enable = true;

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.configurationLimit = 5;
  boot.loader.efi.canTouchEfiVariables = true;

  # Intel graphics (UHD 620).
  services.xserver.videoDrivers = [ "modesetting" ];
  hardware.graphics.enable = true;

  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
  };
  services.blueman.enable = true;

  # Laptop power management.
  services.tlp = {
    enable = lib.mkDefault true;
    settings = {
      CPU_SCALING_GOVERNOR_ON_AC = "powersave";
      CPU_SCALING_GOVERNOR_ON_BAT = "powersave";
      CPU_ENERGY_PERF_POLICY_ON_AC = "balance_performance";
      CPU_ENERGY_PERF_POLICY_ON_BAT = "power";
      CPU_BOOST_ON_AC = 1;
      CPU_BOOST_ON_BAT = 0;
      PLATFORM_PROFILE_ON_AC = "balanced";
      PLATFORM_PROFILE_ON_BAT = "low-power";
      WIFI_PWR_ON_BAT = "on";
      RUNTIME_PM_ON_BAT = "auto";
      USB_AUTOSUSPEND = 1;
      STOP_CHARGE_THRESH_BAT0 = 85;
    };
  };

  services.thermald.enable = true;
  services.upower.enable = lib.mkDefault true;
  services.fwupd.enable = true;
  services.fstrim.enable = true;

  # No swap partition; keep some compressed swap in RAM.
  zramSwap.enable = true;

  services.openssh = {
    enable = true;
    settings.PasswordAuthentication = true;
  };

  environment.systemPackages = with pkgs; [
    powertop
    acpi
    brightnessctl
  ];

  # This value determines the NixOS release with which the machine was
  # installed. Do not change it unless you have read the migration notes.
  system.stateVersion = "26.05";
}
