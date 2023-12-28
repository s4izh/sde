# PS1='\[\e[97m\]\u\[\e[39m\]:\[\e[97m\]\W\[\e[15m\] $ '
PS1='\u:\W $ '

if [ -z "$EMACS" ]; then
    set -o vi
    bind -m vi-command 'Control-l:clear-screen'
    bind -m vi-insert 'Control-l:clear-screen'
fi
