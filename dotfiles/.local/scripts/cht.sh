#!/bin/sh

# curl cht.sh/$1/$2

languages=$(echo "golang lua cpp c python bash rust" | tr " " "\n")
core_utils=$(echo "curl wget grep sed awk find xargs cut rg" | tr " " "\n")
selected=$(printf "$languages\n$core_utils" | fzf)

query=$(echo "" | fzf --print-query)

if printf $languages | grep -qs $selected; then
  tmux neww "curl cht.sh/$selected/$(echo $query | tr " " "+") | less -"
else
  tmux neww "curl cht.sh/$selected~$query"
fi
