#!/bin/sh

# set proper keyboard and mouse settings
setxkbmap es -option ctrl:nocaps
xinput --set-prop 9 295 0 1

# set wallpaper
feh --bg-fill $HOME/pix/system-wallpaper

# picom no slow animations settings
# picom --fade-in-step=1 --fade-out-step=1 --fade-delta=0 &

# resolution settings
# xrandr --output HDMI-0 --off --output DP-0 --mode 3440x1440

# network manager systray
nm-applet --indicator &

# bar
dwmblocks &

# bindings daemon
sxhkd &

# emacs daemon
/usr/bin/emacs --daemon
