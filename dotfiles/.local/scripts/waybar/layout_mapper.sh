#!/usr/bin/env bash

# Function to translate the code
map_layout() {
    case "$1" in
        "S")  echo "󰕰  Scroller" ;;
        "T")  echo "󰕰  Tile"     ;;
        "G")  echo "󰕰  Grid"     ;;
        "M")  echo "󰕰  Monocle"  ;;
        "K")  echo "󰕰  Deck"     ;;
        "VS") echo "󰕰  V-Scroller" ;;
        "VT") echo "󰕰  V-Tile"     ;;
        "CT") echo "󰕰  Center Tile" ;;
        *)    echo "󰕰  $1"       ;; # Fallback to code if unknown
    esac
}

layout="$(mmsg -g -l)"
echo $layout

map_layout "$(mmsg -g -l)"

# Continuous loop using Watch mode (-w)
# We use 'read' to catch every new line from the mmsg stream
mmsg -w -l | while read -r line; do
    map_layout "$line"
done
