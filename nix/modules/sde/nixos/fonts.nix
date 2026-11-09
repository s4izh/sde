{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.sde.fonts;
in
{
  options.sde.fonts = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = config.sde.desktop.enable;
      description = "Enable SDE desktop fonts.";
    };
  };

  config = lib.mkIf cfg.enable {
    nixpkgs.config.input-fonts.acceptLicense = true;
    fonts = {
      packages = with pkgs; [
        iosevka
        jetbrains-mono
        source-code-pro
        source-serif-pro
        unifont_upper
        fantasque-sans-mono
        input-fonts
        fira-code
        commit-mono
        victor-mono
        liberation_ttf
        ibm-plex
        font-awesome
        noto-fonts-color-emoji
        rubik
        comic-mono
        mononoki
        julia-mono
        nerd-fonts._0xproto
      ];
      fontconfig = {
        defaultFonts = {
          serif = [ "Source Serif Pro" ];
        };
      };
    };
  };
}
