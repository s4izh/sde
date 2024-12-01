PS1='\u@\h \w${GUIX_ENVIRONMENT:+ [env]}\n\$ '
PS0='\[\e[2 q\]'

# source the system-wide file
[ -f /etc/bashrc ] && source /etc/bashrc

source_if_exist () { [ -f "$1" ] && source "$1"; }

source_if_exist "$HOME/.config/shell/functions"
source_if_exist "$HOME/.config/shell/aliases"
source_if_exist "$HOME/.config/shell/prompt"
source_if_exist "$HOME/.config/shell/completions"

if command -v direnv > /dev/null 2>&1; then
    eval "$(direnv hook bash)"
fi

if command -v guix > /dev/null 2>&1; then
    source_if_exist "$HOME/.guix-profile/etc/profile"
fi

if [ "$THEME_IS_LIGHT" == "1" ]; then
    export FZF_DEFAULT_OPTS="--color=light"
fi

unset source_if_exist

