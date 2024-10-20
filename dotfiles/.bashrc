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

if [ -f "$HOME/.config/shell/functions" ]; then
    source "$HOME/.config/shell/functions"
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

    local PROMPT_COLOR="$BOLD$GREEN"
    local GIT_COLOR="$NORMAL$YELLOW"
    local NIX_COLOR="$NORMAL$CYAN"
    local GUIX_COLOR="$NORMAL$YELLOW"

    local git_prompt_enabled="no"
    # local PROMPT_CHAR="$NORMAL$GREEN"

    PS1=""

    [[ -n "$SSH_CLIENT" ]] && PS1+="(ssh) "

    if [ "$git_prompt_enabled" == "yes" ]; then
        if git rev-parse --is-inside-work-tree &>/dev/null; then
            toplevel=$(basename $(git rev-parse --show-toplevel))
            relative=$(git rev-parse --show-prefix)
            relative=${relative%/}
            separator="/"; [ -z "$relative" ] && separator=""
            PS1+="\[\e[$PROMPT_COLOR\]$toplevel$separator$relative"
        else
            PS1+="\[\e[$PROMPT_COLOR\]\u@\h:\w"
        fi
    else
        PS1+="\[\e[$PROMPT_COLOR\]\u@\h:\w"
    fi

    if [ -n "$IN_NIX_SHELL" ]; then
        PS1+="\[\e[$NIX_COLOR\] [env]"
    elif [ -n "$GUIX_ENVIRONMENT" ]; then
        PS1+="\[\e[$GUIX_COLOR\] [env] "
    fi

    # PS1+="\[\e[$PROMPT_COLOR\]\u@\h:\w"

    # PS1+="\[\e[$PROMPT_COLOR\]\w"

    # if [ "$git_prompt_enabled" == "yes" ]; then
    #     if git rev-parse --is-inside-work-tree &>/dev/null; then
    #         branch_name=$(git symbolic-ref --short HEAD 2>/dev/null || git describe --tags --exact-match 2>/dev/null)
    #         PS1+=" \[\e[$NORMAL$GIT_COLOR\]*$branch_name\[\e[$NORMAL$PROMPT_COLOR\]"
    #     fi
    # fi

    PS1+=" $ \e[0m"
    # PS1+="\n\[\e[$BOLD$WHITE\]$ \e[0m"
    # PS1+="\n$ \e[0m"
}

# PROMPT_COMMAND=_set_prompt

BOLD="1;"
NORMAL="0;"

RED="31m"
GREEN="32m"
YELLOW="33m"
BLUE="34m"
MAGENTA="35m"
CYAN="36m"
WHITE="37m"

PROMPT_COLOR="$BOLD$GREEN"
GIT_COLOR="$NORMAL$YELLOW"
NIX_COLOR="$NORMAL$CYAN"
GUIX_COLOR="$NORMAL$YELLOW"

# PS1="\[\e[$PROMPT_COLOR\]\u@\h:\w"
# PS1=""
PS1="\[\e[$PROMPT_COLOR\]\w"
# PS1="\[\e[$PROMPT_COLOR\]\W"
# PS1+=" $ \e[0m"
PS1+="\n$ \e[0m"

if command -v direnv > /dev/null 2>&1; then
    eval "$(direnv hook bash)"
fi
