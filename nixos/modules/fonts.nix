{
  config,
  pkgs,
  lib,
  inputs,
  sde,
  ...
}:
{
  nixpkgs.config.input-fonts.acceptLicense = true;
  fonts = {
    packages = with pkgs; [
      iosevka # Iosevka
      jetbrains-mono # JetBrains Mono
      source-code-pro # Source Code Pro
      source-serif-pro # Source Serif Pro
      # unifont # Unifont
      unifont_upper
      # gnu-freefont
      fantasque-sans-mono # Fantasque Sans Mono
      input-fonts
      # fira-code-nerd-font # FiraCode Nerd Font
      # (nerdfonts.override { fonts = [ "LiterationMono Nerd Font Mono" ]; })
      commit-mono # Commit Mono
      # ubuntu-mono # Ubuntu Mono
      # nerdfonts
      texlivePackages.inconsolata-nerd-font
      texlivePackages.gnu-freefont
      victor-mono
      liberation_ttf
      ibm-plex
      font-awesome
      noto-fonts-emoji
      rubik
      comic-mono # Comic Mono
      mononoki # Monoki
      julia-mono
      # FiraCode Nerd Font
    ];
    fontconfig = {
      defaultFonts = {
        #sansSerif = [ "Liberation Mono" ];
        serif = [ "Source Serif Pro" ];
        # monospace = [ "InputMonoCondensed" ];
        # monospace = [ "IBM Plex Mono" ];
        # monospace = [ "FiraCode Nerd Font" ];
        # monospace = [ "LiterationMono Nerd Font" ];
        # monospace = [ "JetBrains Mono" ];
        # monospace = [ "UbuntuMono Nerd Font" ];
        # monospace = [ "Fantasque Sans Mono" ];
        # monospace = [ "Comic Mono" ];
        # monospace = [ "Mononoki" ];
        # monospace = [ "Julia Mono" ];
        monospace = [ "Iosevka" ];
        # monospace = [ "Liberation Mono" ];
        # monospace = [ "Inconsolata Nerd Font" ];
        # monospace = [ "FreeMono" ];
        # monospace = [ "Commit Mono" ];
        # monospace = [ "Victor Mono" ];
        # monospace = [ "Source Code Pro" ];
      };
    };
  };
}
