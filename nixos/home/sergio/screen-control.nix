{ pkgs, ... }:

let
  screen-control = pkgs.writeShellScriptBin "screen-control" ''
    MODE=$1
    SET_VAL=$2
    NOTIFY="${pkgs.libnotify}/bin/notify-send"
    DDC="${pkgs.ddcutil}/bin/ddcutil"

    set_hw_backlight() {
      local level=$1
      MONITORS=$($DDC detect | grep "Display" | cut -d' ' -f2)
      for d in $MONITORS; do
        echo "Adjusting Display $d to $level%..."
        $DDC setvcp 10 "$level" --display "$d"
      done
      wait
    }

    case $MODE in
      day)
        systemctl --user stop auto-brightness.service || true
        pkill gammastep || true
        ${pkgs.gammastep}/bin/gammastep -x > /dev/null 2>&1 || true
        $NOTIFY "Mode: Day" "Colors reset | Backlight 80%"
        set_hw_backlight 80
        ;;
      evening)
        systemctl --user stop auto-brightness.service || true
        pkill gammastep || true
        $NOTIFY "Mode: Evening" "4000K | Backlight 40%"
        ${pkgs.gammastep}/bin/gammastep -m wayland -O 4000 -b 0.8 &
        set_hw_backlight 40
        ;;
      midnight)
        systemctl --user stop auto-brightness.service || true
        pkill gammastep || true
        $NOTIFY "Mode: Midnight" "2500K | Backlight 10%"
        ${pkgs.gammastep}/bin/gammastep -m wayland -O 2500 -b 0.5 &
        set_hw_backlight 10
        ;;
      auto)
        $NOTIFY "Mode: Auto" "Starting Barcelona Sun Schedule"
        systemctl --user start auto-brightness.service
        set_hw_backlight 50
        ;;
      set)
        set_hw_backlight "$SET_VAL"
        $NOTIFY "Manual Brightness" "Set to $SET_VAL%"
        ;;
      stop)
        systemctl --user stop auto-brightness.service || true
        pkill gammastep || true
        ${pkgs.gammastep}/bin/gammastep -x > /dev/null 2>&1 || true
        $NOTIFY "Screen Control" "All adjustments disabled"
        ;;
      *)
        echo "Usage: screen-control [day|evening|midnight|auto|set (0-100)|stop]"
        ;;
    esac
  '';
in
{
  home.packages = [
    screen-control
    pkgs.gammastep
    pkgs.libnotify
  ];

  # systemd.user.services.auto-brightness = {
  #   Unit = {
  #     Description = "Automated screen brightness/color check";
  #     After = [ "graphical-session.target" ];
  #   };
  #   Service = {
  #     Type = "simple";
  #     # ExecStart = "${screen-script}/bin/screen-control";
  #     # ExecStart = "${pkgs.gammastep}/bin/gammastep -m wayland -l 41.3:2.1 -t 6400:3600 -b 0.9:0.6";
  #     ExecStart = ''
  #       ${pkgs.gammastep}/bin/gammastep \
  #         -m wayland \
  #         -l 41.3:2.1 \
  #         -t 6300:3600 \
  #         -b 0.9:0.50 # \
  #         # -g 0.8:0.8:0.8
  #     '';
  #     Restart = "always";
  #     RestartSec = 5;
  #   };
  #   Install = {
  #     WantedBy = [ "graphical-session.target" ];
  #   };
  # };

  # systemd.user.timers.auto-brightness = {
  #   Unit = {
  #     Description = "Check brightness every 5 minutes";
  #   };
  #   Timer = {
  #     OnCalendar = "*:0/5";
  #     Persistent = true;
  #   };
  #   Install = {
  #     WantedBy = [ "timers.target" ];
  #   };
  # };
}
