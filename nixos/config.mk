SDE_DIR := $(PWD)/..
NIX_DIR := $(PWD)
GUIX_DIR := $(SDE_DIR)/guix
DOTFILES := $(SDE_DIR)/dotfiles
SRC_DIR := $(HOME)/.local/src

LINKS_FROM_SDE_TO_HOME := \
	dotfiles/.bashrc:.bashrc \
	dotfiles/.config/alacritty:.config/alacritty \
	dotfiles/.config/dunst:.config/dunst \
	dotfiles/.config/git:.config/git \
	dotfiles/.config/kanshi:.config/kanshi \
	dotfiles/.config/libvirt:.config/libvirt \
	dotfiles/.config/mimeapps.list:.config/mimeapps.list \
	dotfiles/.config/picom:.config/picom \
	dotfiles/.config/river:.config/river \
	dotfiles/.config/shell/bash_profile:.bash_profile \
	dotfiles/.config/shell:.config/shell \
	dotfiles/.config/sway:.config/sway \
	dotfiles/.config/sxhkd:.config/sxhkd \
	dotfiles/.config/tmux:.config/tmux \
	dotfiles/.config/zathura:.config/zathura \
	dotfiles/.config/zsh:.config/zsh \
	dotfiles/.editorconfig:.editorconfig \
	dotfiles/.local/scripts:.local/scripts \
	dotfiles/.xinitrc:.xinitrc \
	dotfiles/.zprofile:.zprofile \
	dotfiles/templates:templates \
	nvim:.config/nvim \
	guix/channels.scm:.config/guix/channels.scm

DIRS_TO_CREATE_IN_HOME := \
	.local \
	.config/guix

GIT_SRCS := \
	https://github.com/s4izh/dwm.git \
	https://github.com/s4izh/dmenu.git \
	https://github.com/s4izh/dwmblocks.git \
	https://github.com/s4izh/dwl.git \
	https://github.com/s4izh/somebar.git \
	https://github.com/s4izh/st.git

