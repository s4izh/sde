SDE_DIR := $(PWD)/..
NIX_DIR := $(PWD)
GUIX_DIR := $(SDE_DIR)/guix
DOTFILES := $(SDE_DIR)/dotfiles
SRC_DIR := $(HOME)/.local/src

LINKS_FROM_SDE_TO_HOME := \
	dotfiles/.bashrc:.bashrc \
	dotfiles/.config/alacritty:.config/alacritty \
	dotfiles/.config/foot:.config/foot \
	dotfiles/.config/git:.config/git \
	dotfiles/.config/hypr:.config/hypr \
	dotfiles/.config/kanshi:.config/kanshi \
	dotfiles/.config/kitty:.config/kitty \
	dotfiles/.config/libvirt:.config/libvirt \
	dotfiles/.config/mango:.config/mango \
	dotfiles/.config/mimeapps.list:.config/mimeapps.list \
	dotfiles/.config/picom:.config/picom \
	dotfiles/.config/river:.config/river \
	dotfiles/.config/shell/bash_profile:.bash_profile \
	dotfiles/.config/shell:.config/shell \
	dotfiles/.config/sway:.config/sway \
	dotfiles/.config/sxhkd:.config/sxhkd \
	dotfiles/.config/tmux:.config/tmux \
	dotfiles/.config/wpaperd:.config/wpaperd \
	dotfiles/.config/xkb:.config/xkb \
	dotfiles/.config/zathura:.config/zathura \
	dotfiles/.editorconfig:.editorconfig \
	dotfiles/.guile:.guile \
	dotfiles/.local/scripts:.local/scripts \
	dotfiles/.xinitrc:.xinitrc \
	dotfiles/templates:templates \
	nvim:.config/nvim \
	guix/channels.scm:.config/guix/channels.scm

# per-host links, falling back to hosts/default.conf if the host has no file
HOST := $(shell hostname)

ifneq ($(wildcard $(DOTFILES)/.config/mango/hosts/$(HOST).conf),)
  LINKS_FROM_SDE_TO_HOME += dotfiles/.config/mango/hosts/$(HOST).conf:.config/mango/host.conf
else
  LINKS_FROM_SDE_TO_HOME += dotfiles/.config/mango/hosts/default.conf:.config/mango/host.conf
endif

DIRS_TO_CREATE_IN_HOME := \
	.local \
	.config/guix \
	.cache/sde \

GIT_SRCS := \
	https://github.com/s4izh/dwm.git \
	https://github.com/s4izh/dmenu.git \
	https://github.com/s4izh/dwmblocks.git \
	https://github.com/s4izh/dwl.git \
	https://github.com/s4izh/somebar.git \
	https://github.com/s4izh/st.git

