{ config, pkgs, lib, ... }:
{
  xdg = {
    enable = true;
    userDirs = {
      enable = true;
      createDirectories = true;
    };
  };

  programs.git = {
    enable = true;
    #userName = "s4izh";
    #userEmail = "sergiosanz234@gmail.com";
    #defaultBranch = "main";
  };

  xdg.configFile."git/config".source = ./.config/git/config;

  # xdg.configFile.<name>.recursive

  programs.ssh = {
    enable = true;
    matchBlocks = {
      "github.com" = {
        hostname = "github.com";
        user = "git";
        identityFile = "~/.ssh/github";
      };
    };
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
    extraConfig = ''
      (setq user-emacs-directory "/home/sergio/.config/emacs/")
    '';
  };

  home.stateVersion = "22.11";
}
