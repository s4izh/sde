#!/usr/bin/env bash

function run_bg() {
    pid=$(pgrep "$1")
    if [ -z "$pid" ]; then
        "$@" &
    else
        pkill "$1"
        "$@" &
    fi
}

apps=(
    "kanshi"
    "waybar"
    "swayosd-server"
    "swaync"
    "nm-applet --indicator"
)

for app in "${apps[@]}"; do
    pname=$(echo "$app" | awk '{print $1}')
    run_bg "$app"
done

swaybg -i /path/to/wall.png &
