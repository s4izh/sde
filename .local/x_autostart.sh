#!/bin/bash

#keyboard settings
# setxkbmap es -option ctrl:nocaps
setxkbmap es -option caps:escape

# set wallpaper
feh --bg-fill $HOME/pix/system-wallpaper

# picom no slow animations settings
# picom --fade-in-step=1 --fade-out-step=1 --fade-delta=0 &

# network manager systray
nm-applet --indicator &

# bar
dwmblocks &

# bindings daemon
sxhkd &

# emacs daemon
# /usr/bin/emacs --daemon

dunst &

# host specific configuration
hostname=$(cat /etc/hostname)

case "$hostname" in
    'zen') # laptop
        # xinput --set-prop 11 290 1
        # xinput --set-prop 10 298 1
    ;;
    'arch') # casa
	xinput --set-prop 14 293 0 1
        # resolution settings
        # xrandr --output HDMI-0 --off --output DP-0 --mode 3440x1440
    ;;
esac


