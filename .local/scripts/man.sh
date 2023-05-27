#!/bin/sh

# curl cht.sh/$1/$2

manpage=$(man -k . | fzf -i)

query="$(echo "$manpage" | awk '{print $2 $1}' | sed 's/(//' | tr ')' ' ')"

if [ -z "$query" ]; then
    exit 1
fi

tmux neww -n man "man $query"
# tmux split-window -h "man $query"
