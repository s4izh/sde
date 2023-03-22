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

  programs.ssh = {
    enable = true;
    matchBlocks = {
      "github.com" = {
        hostname = "github.com";
        user = "git";
        identityFile = "/home/sergio/.ssh/github";
      };
    };
  };


  home.stateVersion = "22.11";
}
