{
  config,
  pkgs,
  lib,
  sde,
  ...
}:
let
  cfg = config.sde.theming;
  gtkExtraCss = builtins.readFile "${sde.flakeRoot}/dotfiles/.config/gtk-3.0/gtk.css";
in
{
  # GTK theme and cursor for standalone WMs. Full desktop environments
  # (Plasma, GNOME) manage these themselves and break on the read-only files
  # home-manager writes, so the NixOS side turns this off through
  # sde.desktop.fullEnvironment.
  options.sde.theming.enable = lib.mkOption {
    type = lib.types.bool;
    default = true;
    description = "Whether home-manager manages the GTK theme and cursor.";
  };

  config = lib.mkIf cfg.enable {
    gtk = {
      enable = true;
      theme = {
        name = "adw-gtk3-dark";
        package = pkgs.adw-gtk3;
      };
      iconTheme = {
        # name = "Papirus-dark";
        name = "Adwaita";
        # package = pkgs.papirus-icon-theme;
        package = pkgs.adwaita-icon-theme;
      };
      cursorTheme = {
        name = "Adwaita";
        package = pkgs.adwaita-icon-theme;
      };
      font = {
        name = "monospace";
        size = 10;
      };
      gtk3.extraConfig = {
        gtk-application-prefer-dark-theme = true;
        # gtk-application-prefer-light-theme = true;
        # gtk-dialogs-use-header = false;
        # gtk-decoration-layout= ":";
      };
      gtk3.extraCss = gtkExtraCss;
      gtk4.extraConfig = {
        gtk-application-prefer-dark-theme = true;
        # gtk-application-prefer-light-theme = true;
        # gtk-dialogs-use-header = false;
        # gtk-decoration-layout= ":";
      };
      gtk4.extraCss = gtkExtraCss;
      gtk4.theme = config.gtk.theme;
      gtk3.bookmarks = [
        "file:///mnt"
        "file:///home/sergio/notes"
        "file:///home/sergio/notes/docs"
        "file:///home/sergio/uni/miri"
        "file:///home/sergio/uni/mei"
        "file:///home/sergio/uni/gei"
        "file:///home/sergio/personal/tfm"
      ];
    };

    # copiado del Toomoch
    home.pointerCursor = {
      enable = true;
      name = "Adwaita";
      package = pkgs.adwaita-icon-theme;
      size = 24;
      x11 = {
        enable = true;
        defaultCursor = "Adwaita";
      };
      gtk.enable = true;
      # also link the theme into ~/.icons, some older apps only look there
      dotIcons.enable = true;
    };
  };
}
