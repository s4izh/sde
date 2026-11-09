{
  config,
  pkgs,
  lib,
  ...
}:
let
  # session menu: SUPER+SHIFT+q in mango and waybar's power button.
  # entries carry an icon from the active icon theme (fuzzel's dmenu
  # "label\0icon\x1fname"), the ones that end the session ask first
  powerMenu = pkgs.writeShellApplication {
    name = "fuzzelpoweroffmenu";
    # procps first, coreutils also has an uptime without -p
    runtimeInputs = with pkgs; [
      procps
      coreutils
      fuzzel
      gnused
      systemd
      swaylock
    ];
    text = ''
      entry() { printf '%s\0icon\x1f%s\n' "$1" "$2"; }

      menu() {
        fuzzel --dmenu --index --prompt "$1" --lines "$2" --width 30 --placeholder "$3"
      }

      confirm() {
        local i
        i=$({ entry "$1" "$2"; entry "Cancel" "window-close"; } | menu "$1? " 2 "")
        [ "$i" = 0 ]
      }

      up=$(uptime -p | sed 's/^up //')

      i=$({
        entry "Lock" "system-lock-screen"
        entry "Suspend" "system-suspend"
        entry "Log out" "system-log-out"
        entry "Reboot" "system-reboot"
        entry "Power off" "system-shutdown"
      } | menu "  " 5 "up $up") || exit 0

      case "$i" in
        0) swaylock -f ;;
        1) swaylock -f && systemctl suspend ;;
        # mmsg comes with mango, not pinned here
        2) confirm "Log out" "system-log-out" && mmsg dispatch quit ;;
        3) confirm "Reboot" "system-reboot" && systemctl reboot ;;
        4) confirm "Power off" "system-shutdown" && systemctl poweroff ;;
      esac
    '';
  };
in
{
  # colors, font and icon theme come from theming.nix, they follow the
  # theme and the light/dark mode. fuzzel reads its config on every launch,
  # so a theme-mode toggle shows up the next time it opens
  programs.fuzzel = {
    enable = true;
    settings = {
      main = {
        terminal = "kitty";
        layer = "overlay";
        lines = 12;
        width = 40;
        horizontal-pad = 16;
        vertical-pad = 12;
        inner-pad = 8;
      };
      border = {
        width = 2;
        radius = 8;
      };
    };
  };

  home.packages = [ powerMenu ];
}
