#!/bin/bash

#keyboard settings
setxkbmap es -option ctrl:nocaps

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
/usr/bin/emacs --daemon

hostname=$(cat /etc/hostname)

case "$hostname" in
    'zen') # laptop
        if [ "$(xinput | wc -l)" == "13"]; then
            xinput --set-prop 11 290 1
        fi
    ;;
    'arch') # casa
        # TODO
        # resolution settings
        # xrandr --output HDMI-0 --off --output DP-0 --mode 3440x1440
    ;;
esac


