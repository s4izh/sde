#!/usr/bin/env bash
source "$HOME/.local/scripts/lib/sde.sh"

PROJECT_PATH="$HOME/vpn"
SESSION_NAME="vpn"

if ! tmux_has "$SESSION_NAME"; then
    tmux new-session -d -s "$SESSION_NAME" -n 'dashboard' -c "$PROJECT_PATH"
    tmux send-keys -t "$SESSION_NAME:dashboard" "nmcli connection show --active" C-m

    tmux new-window -t "$SESSION_NAME" -n 'upc' -c "$PROJECT_PATH"
    tmux send-keys -t "$SESSION_NAME:upc" "sudo openfortivpn myupclink.upc.edu:443 --saml-login"

    tmux new-window -t "$SESSION_NAME" -n 'bsc' -c "$PROJECT_PATH"
    tmux send-keys -t "$SESSION_NAME:bsc" "bsc-vpn"

    tmux new-window -t "$SESSION_NAME" -n 'work' -c "$PROJECT_PATH"
    tmux send-keys -t "$SESSION_NAME:work" "sudo openfortivpn -c lanaccess.conf"

    tmux select-window -t "$SESSION_NAME:dashboard"
fi

tmux_go "$SESSION_NAME"
