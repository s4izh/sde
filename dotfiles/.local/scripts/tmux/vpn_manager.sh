#!/usr/bin/env bash

PROJECT_PATH="$HOME/vpn"
SESSION_NAME="vpn"

if ! tmux has-session -t "$SESSION_NAME" 2>/dev/null; then
    tmux new-session -d -s "$SESSION_NAME" -n 'dashboard' -c "$PROJECT_PATH"
    tmux send-keys -t "$SESSION_NAME:dashboard" "nmcli connection show --active" C-m

    tmux new-window -t "$SESSION_NAME" -n 'upc' -c "$PROJECT_PATH"
    tmux send-keys -t "$SESSION_NAME:upc" "sudo openfortivpn myupclink.upc.edu:443 --saml-login"

    tmux new-window -t "$SESSION_NAME" -n 'bsc' -c "$PROJECT_PATH"
    # BSC_VPN_GATEWAY / BSC_VPN_USER come from sde-private (rc.d/private.bash);
    # single quotes so the pane's own shell expands them.
    tmux send-keys -t "$SESSION_NAME:bsc" 'sudo openfortivpn "${BSC_VPN_GATEWAY:?not set, see sde-private}" -u "${BSC_VPN_USER:?not set, see sde-private}"'

    tmux new-window -t "$SESSION_NAME" -n 'work' -c "$PROJECT_PATH"
    tmux send-keys -t "$SESSION_NAME:work" "sudo openfortivpn -c lanaccess.conf"

    tmux select-window -t "$SESSION_NAME:dashboard"
fi

if [[ -z $TMUX ]]; then
    tmux attach-session -t "$SESSION_NAME"
else
    tmux switch-client -t "$SESSION_NAME"
fi
