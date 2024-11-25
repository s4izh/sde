.DEFAULT_GOAL := help

MODULES=modules

.PHONY: home system system-with-url home-container src-dependencies create-softlinks remove-softlinks

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| sort \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

system: ## system reconfigure
	sudo guix system -L $(MODULES) reconfigure deploys/$(HOSTNAME)/config.scm

system-with-urls: ## system reconfigure with explicit url susbsitutes
	sudo guix system reconfigure hosts/$(HOSTNAME)/config.scm --substitute-urls='https://ci.guix.gnu.org https://bordeaux.guix.gnu.org https://substitutes.nonguix.org'

home: ## reconfigure home environment
	RUNNING_GUIX_HOME=1 guix home -L $(MODULES) reconfigure deploys/$(HOSTNAME)/config.scm

home-container: ## container with home environment
	RUNNING_GUIX_HOME=1 guix home -L $(MODULES) container deploys/$(HOSTNAME)/config.scm 

repl:
	guix repl -L modules

SDE_DIR := $(HOME)/personal/sde
NIX_DIR := $(SDE_DIR)/nixos
DOTFILES := $(SDE_DIR)/dotfiles
SRC_DIR := $(HOME)/.local/src

HOSTNAME ?= $(shell hostname)

GIT_SRCS := \
	https://github.com/s4izh/dwm.git \
	https://github.com/s4izh/dmenu.git \
	https://github.com/s4izh/dwmblocks.git \
	https://github.com/s4izh/dwl.git \
	https://github.com/s4izh/somebar.git \
	https://github.com/s4izh/st.git

CONFIG_FILES := \
	.local/scripts \
	.config/alacritty \
	templates \
	.config/zsh \
	.zprofile \
	.bashrc \
	.xinitrc \
	.editorconfig \
	.config/tmux \
	.config/sway \
	.config/shell \
	.config/git \
	.config/libvirt \
	.config/zathura \
	.config/picom \
	.config/sxhkd \
	.config/dunst

REPO_NAMES := $(notdir $(basename $(GIT_SRCS)))

