# Set up Guix Home profile
if [ -f ~/.profile ]; then . ~/.profile; fi

# Honor per-interactive-shell startup file
if [ -f ~/.bashrc ]; then . ~/.bashrc; fi

# GUIX specific
if command -v guix &> /dev/null; then
    # Merge search-paths from multiple profiles, the order matters.
    eval "$(guix package --search-paths \
    -p $HOME/.config/guix/current \
    -p $HOME/.guix-home/profile \
    -p $HOME/.guix-profile \
    -p /run/current-system/profile)"
    # Prepend setuid programs.
    export PATH=/run/setuid-programs:$PATH
fi

if [[ "$(tty)" =~ ^/dev/tty ]] || [ -n "$TMUX" ]; then
    if [ -f "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
fi

export PATH="$HOME/.local/bin":$PATH
export PATH="$HOME/.local/scripts:$PATH"
export PATH="$HOME/.local/scripts/statusbar:$PATH"
export PATH="$HOME/.local/scripts/tmux":$PATH
export TERM=xterm-256color
