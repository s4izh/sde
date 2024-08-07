#PS1='\u:\W $ '

if [ -z "$INSIDE_EMACS" ]; then
    set -o vi
    bind -m vi-command 'Control-l:clear-screen'
    bind -m vi-insert 'Control-l:clear-screen'
    eval "$(fzf --bash)"
fi

BOLD_RED="1;31m"
BOLD_GREEN="1;32m"
BOLD_WHITE="1;37m"

if [ "$TERM" != "dumb" ] || [ -n "$INSIDE_EMACS" ]; then
  PROMPT_COLOR=$BOLD_RED # rojo para el root
  ((UID)) && PROMPT_COLOR=$BOLD_WHITE
  if [ -n "$INSIDE_EMACS" ]; then
    # Emacs term mode doesn't support xterm title escape sequence (\e]0;)
    PS1="\n\[\033[$PROMPT_COLOR\][\u@\h:\w]\\$\[\033[0m\] "
  else
    PS1="\n\[\033[$PROMPT_COLOR\][\[\e]0;\u@\h: \w\a\]\u@\h:\w]\\$\[\033[0m\] "
  fi
  if test "$TERM" = "xterm"; then
    PS1="\[\033]2;\h:\u:\w\007\]$PS1"
  fi
fi

if [ -f "$HOME/.config/shell/aliases" ]; then
    source "$HOME/.config/shell/aliases"
fi
