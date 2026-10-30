{
  config,
  pkgs,
  lib,
  ...
}:

{
  programs.waybar = {
    enable = true;
    settings = {
      mainBar = {
        # output = [ "DP-3" "HDMI-A-1" ];
        layer = "top";
        height = 10;
        spacing = 1;
        modules-left = [
          # "sway/workspaces"
          # "sway/mode"
          # "sway/window"
          # "river/tags"
          # mango >= 0.18 dropped dwl-ipc, tags come through ext-workspace-v1
          "ext/workspaces"
          # "dwl/tags"
          # "dwl/window"
          # "dwl/layout"
          # "custom/wm_layout"
          # "river/tags"
          # "dwl/tags"
          # "dwl/window"
          # "hyprland/workspaces"
          # "hyprland/window"
          # "river/window"
          # "river/layout"
        ];
        # modules-center = [
        #   "wlr/language"
        # ];
        modules-right = [
          "tray"
          "pulseaudio"
          "network"
          "cpu"
          "memory"
          "temperature"
          "backlight"
          "battery"
          "clock"
          "custom/power"
        ];
        "keyboard-state" = {
          "numlock" = true;
          "capslock" = true;
          "format" = "{name} {icon}";
          "format-icons" = {
            "locked" = "";
            "unlocked" = "";
          };
        };
        "custom/wm_layout" = {
          exec = "python3 ~/.local/scripts/waybar/layout.py {name} --debug 2>&1 | tee /tmp/layout.log";
          format = "{}";
          tooltip = false;
          restart-interval = 1;
        };
        "sway/mode" = {
          "format" = ''<span style="italic">{}</span>'';
        };
        "sway/window" = {
          "format" = "{title}";
          "max-length" = 50;
          "icon" = true;
        };
        "river/window" = {
          "format" = "{title}";
          "max-length" = 50;
          "icon" = true;
        };
        "ext/workspaces" = {
          "format" = "{name}";
          "on-click" = "activate";
          "ignore-hidden" = true;
          "sort-by-id" = true;
        };
        "hyprland/workspaces" = {
          "format" = "{name}";
          "all-outputs" = false;
          "active-only" = false;
        };
        "hyprland/window" = {
          "format" = "{}";
          "separate-outputs" = true;
          "max-length" = 200;
        };
        "tray" = {
          "spacing" = 10;
        };
        "clock" = {
          "format" = "{:%d/%m/%y %H:%M}  ";
          "format-alt" = "{:%A, %B %d, %Y (%R)}";
          "tooltip-format" = "<tt><small>{calendar}</small></tt>";
          "calendar" = {
            "mode" = "year";
            "mode-mon-col" = 3;
            "weeks-pos" = "right";
            "on-scroll" = 1;
            "format" = {
              "months" = "<span color='#ffead3'><b>{}</b></span>";
              "days" = "<span color='#ecc6d9'><b>{}</b></span>";
              "weeks" = "<span color='#99ffdd'><b>W{}</b></span>";
              "weekdays" = "<span color='#ffcc66'><b>{}</b></span>";
              "today" = "<span color='#ff6699'><b><u>{}</u></b></span>";
            };
          };
          "actions" = {
            "on-click-right" = "mode";
            "on-click-forward" = "tz_up";
            "on-click-backward" = "tz_down";
            "on-scroll-up" = "shift_up";
            "on-scroll-down" = "shift_down";
          };
        };
        "cpu" = {
          # "format" = " {usage}% ";
          "format" = "cpu: {usage}%";
        };
        # "memory" = { "format" = "{}% "; };
        "memory" = {
          "format" = "mem: {}%";
        };
        "temperature" = {
          "critical-threshold" = 80;
          "format" = "{temperatureC}°C";
          # "format" = "{temperatureC}°C {icon}";
          # "format-icons" = [ "" "" "" ];
        };
        "backlight" = {
          "format" = "{percent}% {icon}";
          "format-icons" = [
            ""
            ""
          ];
        };
        "battery" = {
          "states" = {
            "warning" = 30;
            "critical" = 15;
          };
          "format" = "{capacity}% {icon}";
          "format-charging" = "{capacity}% ";
          "format-plugged" = "{capacity}% ";
          "format-alt" = "{time} {icon}";
          "format-icons" = [
            ""
            ""
            ""
            ""
            ""
          ];
        };
        "network" = {
          "format-wifi" = "{essid} ({signalStrength}%) ";
          "format-ethernet" = "{ipaddr}/{cidr} ";
          "tooltip-format" = "{ifname} via {gwaddr} ";
          "format-linked" = "{ifname} (No IP) ";
          "format-disconnected" = "Disconnected ⚠";
          "format-alt" = "{ifname}: {ipaddr}/{cidr}";
        };
        "pulseaudio" = {
          "format" = "{volume}% {icon} {format_source}";
          "format-bluetooth" = "{volume}% {icon} {format_source}";
          "format-bluetooth-muted" = " {icon} {format_source}";
          "format-muted" = " {format_source}";
          "format-source" = "{volume}% ";
          "format-source-muted" = "";
          "format-icons" = {
            "headphone" = "";
            "hands-free" = "";
            "headset" = "";
            "phone" = "";
            "portable" = "";
            "car" = "";
            "default" = [
              ""
              ""
              ""
            ];
          };
          "on-click" = "pavucontrol";
        };
        "custom/power" = {
          "format" = "";
          "on-click" = "fuzzelpoweroffmenu";
        };
      };
    };

    # waybar-style.css is the dark bar, the light mode (sde.theming.polarity)
    # overrides its base colors. theme-mode reloads waybar with SIGUSR2
    style =
      builtins.readFile ./waybar-style.css
      + lib.optionalString (config.sde.theming.enable && config.sde.theming.polarity == "light") ''

        /* light mode */
        window#waybar {
          background-color: rgba(250, 250, 250, 0.85);
          border-bottom: 3px rgba(250, 250, 250, 0.85);
          color: #1e1e1e;
        }

        button:hover {
          box-shadow: inset 0 -3px #1e1e1e;
        }

        #workspaces button,
        #clock,
        #battery,
        #cpu,
        #memory,
        #disk,
        #temperature,
        #backlight,
        #network,
        #pulseaudio,
        #custom-media,
        #custom-power,
        #tray,
        #mode,
        #idle_inhibitor,
        #scratchpad,
        #mpd {
          color: #1e1e1e;
        }

        #workspaces button:hover {
          background: rgba(0, 0, 0, 0.1);
        }

        #workspaces button.visible:not(.focused) {
          background-color: #dcdcdc;
          box-shadow: inset 0 -3px #1e1e1e;
        }

        #workspaces button.focused,
        #workspaces button.active {
          background-color: #b9c6cf;
          box-shadow: inset 0 -3px #1e1e1e;
        }

        #battery.charging,
        #battery.plugged {
          color: #0a8a3a;
        }
      '';
  };
}
