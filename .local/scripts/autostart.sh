#!/bin/sh

setxkbmap es -option caps:escape
xinput --set-prop 10 295 0 1
sxhkd &

nm-applet --indicator &
feh --bg-fill $(find $HOME/pix/wall -type f | shuf -n 1) &


