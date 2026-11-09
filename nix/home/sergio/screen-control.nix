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

  # Monitor power control, two independent mechanisms:
  #
  #   1. wlopm   - asks the compositor (zwlr-output-power-management-v1) to stop
  #                driving the output. Instant, and mango implements it.
  #   2. ddcutil - DDC/CI VCP feature D6 straight to the panel over i2c.
  #               ~2s, but works with no Wayland session at all (tty, other WM).
  #
  # wlopm is preferred; ddcutil is the fallback when there is no compositor to
  # talk to. Neither wakes on mouse/keyboard by itself — that is an idle-daemon
  # job — so both are symmetric press-to-sleep / press-to-wake.
  monitor-toggle = pkgs.writeShellScriptBin "monitor-toggle" ''
    set -u
    WLOPM="${pkgs.wlopm}/bin/wlopm"
    DDC="${pkgs.ddcutil}/bin/ddcutil"

    # D6: 0x01 = on, 0x04 = off (DPMS active-off, recoverable over i2c).
    # Deliberately NOT 0x05 (hard off): most panels cut the DDC circuit at that
    # level and only come back via the physical power button.
    ON=01
    OFF=04

    have_compositor() {
      [ -n "''${WAYLAND_DISPLAY:-}" ] && $WLOPM >/dev/null 2>&1
    }

    STATE_DIR="''${XDG_RUNTIME_DIR:-/tmp}/monitor-toggle"
    BUS_CACHE="$STATE_DIR/buses"
    mkdir -p "$STATE_DIR"

    # Detection needs the monitors awake, so cache the bus list while they are.
    # Waking up then never depends on being able to see them first.
    #
    # Crucially the cache may only ever GROW: a detect run while some panels are
    # asleep (e.g. during `on`) sees only the subset that still answers i2c. If
    # that subset overwrote the cache we would wake just those — the exact bug of
    # "off kills all, on revives one". So we union freshly-detected buses with
    # the cache and keep the superset that drives every power command.
    buses() {
      local found
      found=$($DDC detect --brief 2>/dev/null \
        | grep -oP 'I2C bus:\s+/dev/i2c-\K[0-9]+')

      printf '%s\n%s\n' "$found" "$(cat "$BUS_CACHE" 2>/dev/null)" \
        | grep -E '^[0-9]+$' | sort -n -u > "$BUS_CACHE.tmp"
      mv "$BUS_CACHE.tmp" "$BUS_CACHE"
      cat "$BUS_CACHE"
    }

    set_power() {
      local val=$1 bus
      for bus in $BUSES; do
        # --noverify: reading back a panel that just powered off stalls.
        $DDC setvcp D6 "$val" --bus "$bus" --noverify >/dev/null 2>&1 &
      done
      wait
    }

    ACTION=''${1:-toggle}

    case $ACTION in
      toggle|on|off|status) ;;
      *)
        echo "Usage: monitor-toggle [toggle|on|off|status] [--ddc]" >&2
        exit 1
        ;;
    esac

    # --ddc forces the i2c path, for testing or when the compositor lies.
    if [ "''${2:-}" != "--ddc" ] && have_compositor; then
      case $ACTION in
        toggle) exec $WLOPM --toggle '*' ;;
        on)     exec $WLOPM --on '*'     ;;
        off)    exec $WLOPM --off '*'    ;;
        status) exec $WLOPM              ;;
      esac
    fi

    # ---- fallback: DDC/CI over i2c ----
    BUSES=$(buses)
    if [ -z "$BUSES" ]; then
      echo "monitor-toggle: no compositor, and no DDC displays or cached buses" >&2
      exit 1
    fi

    case $ACTION in
      on)  set_power $ON  ;;
      off) set_power $OFF ;;
      toggle)
        # Probe one display only; a failed read means it is already asleep,
        # so the fallback direction is "turn on" — the recoverable one.
        if $DDC getvcp D6 --bus "''${BUSES%%$'\n'*}" 2>/dev/null | grep -q 'sl=0x01'; then
          set_power $OFF
        else
          set_power $ON
        fi
        ;;
      status)
        for bus in $BUSES; do
          printf 'i2c-%s: %s\n' "$bus" \
            "$($DDC getvcp D6 --bus "$bus" 2>/dev/null | grep -oP 'sl=\K0x[0-9a-f]+' || echo unreachable)"
        done
        ;;
    esac
  '';
in
{
  home.packages = [
    screen-control
    monitor-toggle
    pkgs.wlopm
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
