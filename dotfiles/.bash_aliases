#!/bin/sh

alias cfz='cd ~/.config/zsh && nvim .zshrc'
alias cfv='cd ~/.config/nvim && nvim $(fzf)'
alias qtilecf='cd ~/.config/qtile && nvim .'
alias cfi='cd ~/.config/i3 && nvim config'
alias cfd='cd ~/.local/src/dwm && nvim config.h'
alias cfdl='cd ~/.local/src/dwl && nvim config.h'
alias cfh='cd ~/.config/hypr && nvim hyprland.conf'
alias cff='nvim ~/.config/foot/foot.ini'
alias cft='nvim ~/.config/tmux/tmux.conf'

alias cl='clear'

alias em='emacsclient -nw'
alias vim='nvim'

alias tn='tmux-notes'

alias zsh-update-plugins="find "$ZDOTDIR/plugins" -type d -exec test -e '{}/.git' ';' -print0 | xargs -I {} -0 git -C {} pull -q"

# get fastest mirrors
alias mirror="sudo reflector -f 30 -l 30 --number 10 --verbose --save /etc/pacman.d/mirrorlist"
alias mirrord="sudo reflector --latest 50 --number 20 --sort delay --save /etc/pacman.d/mirrorlist"
alias mirrors="sudo reflector --latest 50 --number 20 --sort score --save /etc/pacman.d/mirrorlist"
alias mirrora="sudo reflector --latest 50 --number 20 --sort age --save /etc/pacman.d/mirrorlist"

# colorize grep output (good for log files)
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'

# confirm before overwriting something
# alias cp="cp -i"
# alias mv='mv -i'
# alias rm='rm -i'

# easier to read disk
alias df='df -h'     # human-readable sizes
alias free='free -m' # show sizes in MB

# get top process eating memory
alias psmem='ps auxf | sort -nr -k 4 | head -5'

# get top process eating cpu ##
alias pscpu='ps auxf | sort -nr -k 3 | head -5'

# gpg encryption
# verify signature for isos
alias gpg-check="gpg2 --keyserver-options auto-key-retrieve --verify"
# receive the key of a developer
alias gpg-retrieve="gpg2 --keyserver-options auto-key-retrieve --receive-keys"

# For when keys break
alias arch-fix-keys="sudo pacman-key --init && sudo pacman-key --populate archlinux && sudo pacman-key --refresh-keys"

# systemd
alias systemctl-list="systemctl list-unit-files --state=enabled"

alias sz="source ~/.bashrc"

# git
alias gm="git checkout main"
alias gco="git checkout"
alias gc="git commit -m"
alias gst="git status"
alias ga="git add"
alias gsu="git submodule update --init --recursive"
alias gsr="git submodule update --recursive --remote"

alias index="nvim index*"

alias \
	cp="cp -iv" \
	mv="mv -iv" \
	rm="rm -vI" \
	mkd="mkdir -pv" \
	yt="yt-dlp --embed-metadata -i" \
	yta="yt -x -f bestaudio/best" \
	ffmpeg="ffmpeg -hide_banner"


# colorize commands when possible
alias \
	ls="ls --color=auto --group-directories-first" \
	grep="grep --color=auto" \
	diff="diff --color=auto" \
	ccat="highlight --out-format=ansi" \
	ip="ip -color=auto"

# abbreviated long commands
alias \
	ka="killall" \
	g="git" \
	trem="transmission-remote" \
	YT="youtube-viewer" \
	sdn="shutdown -h now" \
	e="$EDITOR" \
	v="nvim" \
	# za="zathura"

# tmux
alias \
    tks="tmux kill-session" \
    tkS="tmux kill-server" \

alias \
    tm='~/.local/scripts/tmux/tmux-main' \
    tp='~/.local/scripts/tmux/tmux-picker' \
    ts='~/.local/scripts/tmux/tmux-sessionizer' \
    tw='~/.local/scripts/tmux/tmux-notes2' \
