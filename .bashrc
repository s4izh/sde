#PS1='\u:\W $ '

if [ -z "$INSIDE_EMACS" ]; then
    set -o vi
    bind -m vi-command 'Control-l:clear-screen'
    bind -m vi-insert 'Control-l:clear-screen'
    eval "$(fzf --bash)"
fi

if [ -f "$HOME/.config/shell/aliases" ]; then
    source "$HOME/.config/shell/aliases"
fi

_set_prompt() {
    local BOLD="1;"
    local NORMAL="0;"

    local RED="31m"
    local GREEN="32m"
    local YELLOW="33m"
    local BLUE="34m"
    local MAGENTA="35m"
    local CYAN="36m"
    local WHITE="37m"

    local PROMPT_COLOR="$BOLD$WHITE"
    local GIT_COLOR="$NORMAL$YELLOW"
    local NIX_COLOR="$NORMAL$CYAN"
    local PROMPT_CHAR="$NORMAL$GREEN"

    if [ -n "$IN_NIX_SHELL" ]; then
        PS1="\[\e[$NIX_COLOR\][nix-shell]\[\e[$PROMPT_COLOR\] \u@\h:\w"
    else
        PS1="\[\e[$PROMPT_COLOR\]\u@\h:\w"
    fi

    if git rev-parse --is-inside-work-tree &>/dev/null; then
        branch_name=$(git symbolic-ref --short HEAD 2>/dev/null || git describe --tags --exact-match 2>/dev/null)
        PS1+=" \[\e[$NORMAL$GIT_COLOR\](git:$branch_name)\[\e[$NORMAL$PROMPT_COLOR\]"
    fi

    # PS1+=" $ \e[0m"
    # PS1+="\n\[\e[$BOLD$WHITE\]$ \e[0m"
    PS1+="\n$ \e[0m"
}

PROMPT_COMMAND=_set_prompt

if command -v direnv > /dev/null 2>&1; then
    eval "$(direnv hook bash)"
fi
