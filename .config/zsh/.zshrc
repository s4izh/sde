#!/bin/zsh

# some useful options (man zshoptions)
setopt extendedglob nomatch menucomplete
setopt interactive_comments
stty stop undef		# Disable ctrl-s to freeze terminal.
zle_highlight=('paste:none') # disable highlighting of pasted text

# beep off
unsetopt BEEP

# completions
autoload -Uz compinit
zstyle ':completion:*' menu select
zstyle ':completion::complete:lsof:*' menu yes select
zmodload zsh/complist
compinit
_comp_options+=(globdots)		# Include hidden files.

# edit current command in $EDITOR
autoload edit-command-line
zle -N edit-command-line
bindkey '^x^e' edit-command-line # ctrl-x ctrl-e

# bindkey -s '^A' '~/.local/scripts/tmux/tmux-main^M'
# bindkey -s '^S' '~/.local/scripts/tmux/tmux-picker^M'
# bindkey -s '^F' '~/.local/scripts/tmux/tmux-sessionizer^M'

alias tm='~/.local/scripts/tmux/tmux-main'
alias tp='~/.local/scripts/tmux/tmux-picker'
alias ts='~/.local/scripts/tmux/tmux-sessionizer'

# Colors
autoload -Uz colors && colors

# Useful Functions
source "$ZDOTDIR/zsh-functions"

# Normal files to source
zsh_add_file "zsh-exports"
zsh_add_file "zsh-vim-mode"
zsh_add_file "zsh-aliases"
# zsh_add_file "zsh-prompt"

# Plugins
# zsh_add_plugin "zsh-users/zsh-autosuggestions"
zsh_add_plugin "zsh-users/zsh-syntax-highlighting"
zsh_add_plugin "hlissner/zsh-autopair"
zsh_add_completion "esc/conda-zsh-completion" false

# Key-bindings
# bindkey -s '^o' 'ranger^M'
# bindkey -s '^a' 'tmux-default^M'
# bindkey -s '^n' 'tmux-notes^M'
bindkey '^R' history-incremental-search-backward

# export FZF_DEFAULT_COMMAND='rg --hidden -l ""'
# eval "$(starship init zsh)"

[ -f "${XDG_CONFIG_HOME:-$HOME/.config}"/fzf/fzf.zsh ] && source "${XDG_CONFIG_HOME:-$HOME/.config}"/fzf/fzf.zsh

# [ -z "$TMUX"  ] && { tmux attach || exec tmux new-session && exit;}

# autoload -U up-line-or-beginning-search
# autoload -U down-line-or-beginning-search
# zle -N up-line-or-beginning-search
# zle -N down-line-or-beginning-search

eval "$(starship init zsh)"
eval "$(direnv hook zsh)"
