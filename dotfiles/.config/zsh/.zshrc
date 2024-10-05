#!/bin/zsh

# some useful options (man zshoptions)
setopt extendedglob nomatch menucomplete
setopt interactive_comments
stty stop undef		# Disable ctrl-s to freeze terminal.
zle_highlight=('paste:none') # disable highlighting of pasted text

setopt auto_cd

# beep off
unsetopt BEEP

# Load aliases and shortcuts if existent.
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/shortcutrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/shortcutrc"
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc"
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/zshnameddirrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/zshnameddirrc"

# completions
autoload -Uz compinit
zstyle ':completion:*' menu select
# zstyle ':completion::complete:lsof:*' menu yes select
zstyle ':completion::*' menu yes select
zstyle ':completion::complete:*' use-cache 1
zmodload zsh/complist
compinit
_comp_options+=(globdots)		# Include hidden files.

# edit current command in $EDITOR
autoload edit-command-line
zle -N edit-command-line
bindkey '^x^e' edit-command-line # ctrl-x ctrl-e

# colors
autoload -Uz colors && colors

# useful Functions
source "$ZDOTDIR/functions"

# normal files to source
zsh_add_file "exports"
zsh_add_file "aliases"
zsh_add_file "prompt"
# zsh_add_file "vim-mode"

if [[ -n $EMACS ]]; then
    export MANPAGER='cat'
else
    zsh_add_file "vim-mode"
fi

# Plugins
zsh_add_plugin "zsh-users/zsh-autosuggestions"
zsh_add_plugin "zsh-users/zsh-syntax-highlighting"
zsh_add_plugin "hlissner/zsh-autopair"
# zsh_add_completion "nix-community/nix-zsh-completions" false
# zsh_add_completion "esc/conda-zsh-completion" false

# key-bindings
bindkey '^R' history-incremental-search-backward

# autoload -U up-line-or-beginning-search
# autoload -U down-line-or-beginning-search
# zle -N up-line-or-beginning-search
# zle -N down-line-or-beginning-search

# use tty-mode when needed
export GPG_TTY=$(tty)
# gpg-connect-agent updatestartuptty /bye >/dev/null

# fzf via package manager
[ -e /usr/share/fzf/key-bindings.zsh ] && source /usr/share/fzf/key-bindings.zsh
[ -e /usr/share/fzf/completion.zsh ] && source /usr/share/fzf/completion.zsh

if [ -n "${commands[fzf-share]}" ]; then
  source "$(fzf-share)/key-bindings.zsh"
  source "$(fzf-share)/completion.zsh"
fi

export FZF_DEFAULT_COMMAND='rg --hidden -l ""'
# fzf via git upstream download
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

eval "$(direnv hook zsh)"
# eval "$(starship init zsh)"

[ -f "/home/sergio/.ghcup/env" ] && source "/home/sergio/.ghcup/env" # ghcup-env
