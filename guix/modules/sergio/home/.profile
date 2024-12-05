export EDITOR=nvim

# guix specific
HOME_ENVIRONMENT=$HOME/.guix-home
if [ -f $HOME_ENVIRONMENT/setup-environment ]; then
    . $HOME_ENVIRONMENT/setup-environment
    $HOME_ENVIRONMENT/on-first-login
fi

if [[ "$(tty)" =~ ^/dev/tty ]] || [ -n "$TMUX" ]; then
    . "$HOME/.bashrc"
fi
