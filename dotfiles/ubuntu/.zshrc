export ZDOTDIR=$HOME/.config/zsh
if [ -d "$HOME/.local/scripts/tmux" ] ; then
    export PATH="$HOME/.local/scripts/tmux:$PATH"
fi
source ~/.config/zsh/.zshrc
