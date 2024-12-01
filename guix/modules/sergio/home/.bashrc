# Bash initialization for interactive non-login shells and
# for remote shells (info "(bash) Bash Startup Files").

# Export 'SHELL' to child processes.  Programs such as 'screen'
# honor it and otherwise use /bin/sh.
export SHELL

if [[ $- != *i* ]]
then
    # We are being invoked from a non-interactive shell.  If this
    # is an SSH session (as in "ssh host command"), source
    # /etc/profile so we get PATH and other essential variables.
    [[ -n "$SSH_CLIENT" ]] && source /etc/profile

    # Don't do anything else.
    return
fi

# Source the system-wide file.
[ -f /etc/bashrc ] && source /etc/bashrc

# ---------- alias ----------------------

alias grep="grep --color=auto"
alias ip="ip -color=auto"
alias ll="ls -l"
alias ls="ls -p --color=auto"
alias vim="nvim"
alias tKs="tmux kill-server"
alias tks="tmux kill-session"
alias ts="tmux-sessionizer"

# -----------------------------------------


# ---------- guix specific ----------------

GUIX_PROFILE="$HOME/.guix-profile"
if [ -f "$GUIX_PROFILE/etc/profile" ]; then
    . "$GUIX_PROFILE/etc/profile"
fi

# -----------------------------------------

PS1='\u@\h \w${GUIX_ENVIRONMENT:+ [env]}\n\$ '

# cursor returns to beam shape always after a command
PS0='\[\e[2 q\]'

eval "$(direnv hook bash)"
