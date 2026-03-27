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
          "dwl/tags"
          "dwl/window"
          # "dwl/layout"
          # "custom/wm_layout"
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

    style = builtins.readFile (./waybar-style.css);
  };
}
