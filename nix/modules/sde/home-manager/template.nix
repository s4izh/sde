{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.sde.example;
in
{
  options.sde.example = {
    enable = lib.mkEnableOption "Template Home Manager module for SDE";

    setting = lib.mkOption {
      type = lib.types.str;
      default = "default-value";
      description = "Example configuration setting.";
    };
  };

  config = lib.mkIf cfg.enable {
    # 1. Packages to add to user profile
    home.packages = with pkgs; [
      # hello
    ];

    # 2. Dotfiles / XDG config files
    # xdg.configFile."example/config.json".text = ''
    #   { "setting": "${cfg.setting}" }
    # '';

    # 3. Environment variables
    # home.sessionVariables = {
    #   EXAMPLE_VAR = cfg.setting;
    # };
  };
}
