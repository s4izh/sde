#!/usr/bin/env bash
# tmux layout for sde, picked up by tmux-sessionizer through the .tmux link.
# One session for both repos, since files move between them and rebuilds run
# from sde/nix:
#
#   sde      repo root (dotfiles, nvim, docs...)
#   nix      sde/nix: flake, hosts, modules; `make switch` from here
#   private  sde-private/nix, only if it is cloned

SESSION_NAME="sde"
SDE_DIR="$HOME/personal/sde"
PRIVATE_DIR="$HOME/personal/sde-private"

attach() {
    if [ -z "$TMUX" ]; then
        tmux attach-session -t "$1"
    else
        tmux switch-client -t "$1"
    fi
}

if tmux has-session -t "=$SESSION_NAME" 2>/dev/null; then
    attach "$SESSION_NAME"
    exit 0
fi

tmux new-session -d -s "$SESSION_NAME" -n sde -c "$SDE_DIR"
tmux new-window -d -t "$SESSION_NAME:" -n nix -c "$SDE_DIR/nix"
if [ -d "$PRIVATE_DIR/nix" ]; then
    tmux new-window -d -t "$SESSION_NAME:" -n private -c "$PRIVATE_DIR/nix"
fi

attach "$SESSION_NAME:sde"
