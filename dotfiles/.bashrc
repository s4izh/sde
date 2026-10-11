# source the system-wide file
[ -f /etc/bashrc ] && source /etc/bashrc

if [[ -f "$HOME"/.bash_profile ]] && [[ -z "$BASH_PROFILE_SOURCED" ]]; then
    # echo "sourcing $HOME/.bash_profile"
    # NOT exported: same-process re-source guard only; see note in bash_profile.
    BASH_PROFILE_SOURCED=1
    source "$HOME"/.bash_profile
fi

# Inside Emacs (M-x shell / comint is a "dumb" terminal): skip cursor-shape
# escapes, readline vi mode, and tty-only widgets that would print garbage.
if [ "$TERM" = "dumb" ] || [ -n "$INSIDE_EMACS" ]; then
    DUMB_SHELL=1
fi

PS1='\u@\h \w${GUIX_ENVIRONMENT:+ [env]}\n\$ '
# steady block cursor; sent before each command so anything that changes the
# cursor shape (nvim, less, ...) gets reset on return to the prompt.
[ -z "$DUMB_SHELL" ] && PS0='\[\e[2 q\]'

source_if_exist () { [ -f "$1" ] && source "$1"; }

source_if_exist "$HOME/.config/shell/functions"
source_if_exist "$HOME/.config/shell/aliases"
source_if_exist "$HOME/.config/shell/prompt"
source_if_exist "$HOME/.config/shell/completions"

if command -v direnv > /dev/null 2>&1; then
    eval "$(direnv hook bash)"
fi

# source only if we aren't already in guix?
# if command -v guix > /dev/null 2>&1; then
#     source_if_exist "$HOME/.guix-profile/etc/profile"
# fi

export GUIX_LOCPATH="$HOME/.guix-profile/lib/locale"

# TODO: use a file for that
if [ "$THEME_IS_LIGHT" == "1" ]; then
    export FZF_DEFAULT_OPTS="--color=light"
fi

export FZF_DEFAULT_OPTS="--color=dark"
# export FZF_DEFAULT_OPTS="--color=base16"
# # export FZF_DEFAULT_OPTS="--color=bw"
# export FZF_DEFAULT_OPTS="--no-color"

if [ -z "$DUMB_SHELL" ] && command -v fzf 2>&1 >/dev/null; then
    eval "$(fzf --bash)"
fi

if [[ -n "$DUMB_SHELL" || -n "$INSIDE_EMACS" ]]; then
    set -o emacs
    PS0=''
fi

unset source_if_exist DUMB_SHELL
