.DEFAULT_GOAL := help

.PHONY: src-dependencies create-softlinks remove-softlinks

SDE_DIR := $(PWD)/..
NIX_DIR := $(PWD)
GUIX_DIR := $(SDE_DIR)/guix
DOTFILES := $(SDE_DIR)/dotfiles
SRC_DIR := $(HOME)/.local/src

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
	.config/mimeapps.list \
	.config/dunst

REPO_NAMES := $(notdir $(basename $(GIT_SRCS)))

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| sort \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

repl: ## Start a nix repl with the flake loaded
	nix repl --expr 'builtins.getFlake (toString ./.)'
test: ## Test the flake
	sudo nixos-rebuild test --flake .# --impure
switch: src-dependencies create-softlinks ## Switch to the flake
	sudo nixos-rebuild switch --flake .\?submodules=1# --impure 
boot:
	sudo nixos-rebuild boot --flake .\?submodules=1# --impure 
bootstrap: src-dependencies create-softlinks ## First bootstrap, specify NIX_HOSTNAME as env variable
	sudo nixos-rebuild switch --flake .#$(NIX_HOSTNAME) --impure
build:
	nixos-rebuild build --flake .\?submodules=1# --impure
update: ## Update the flake
	nix flake update
gc: ## Run garbage collection
	nix-store --gc
user: ## Switch to the flake as a user
	nix build ".#homeManagerConfigurations.sergio.activationPackage" --impure
	./result/activate
	rm -rf result
deploy:
	sudo nixos-rebuild switch --flake ".#thinkcenter" --impure \
		--target-host root@192.168.1.176 --build-host root@192.168.1.176

src-dependencies: ## Download impure source dependencies
	@if [ ! -d $(SRC_DIR) ]; then mkdir -p $(SRC_DIR); fi
	@for repo in $(GIT_SRCS); do \
		repo_name=$$(echo $$repo | sed 's/.*\/\(.*\)\.git/\1/'); \
		if [ ! -d $(SRC_DIR)/$$repo_name ]; then \
			git clone $$repo $(SRC_DIR)/$$repo_name; \
		fi; \
	done

create-softlinks: ## Create all softlinks
	@for file in $(CONFIG_FILES); do \
		if [ ! -h $(HOME)/$$file ]; then \
			ln -svf $(DOTFILES)/$$file $(HOME)/$$file; \
		fi \
	done
	@if [ ! -h $(HOME)/.bash_profile ]; then \
		ln -svf $(DOTFILES)/.config/shell/bash_profile $(HOME)/.bash_profile; \
	fi
	@if [ ! -h $(HOME)/.inputrc ]; then \
		ln -svf $(DOTFILES)/.config/shell/inputrc $(HOME)/.inputrc; \
	fi
	@if [ ! -h $(HOME)/.config/guix/channels.scm ]; then \
		ln -svf $(GUIX_DIR)/channels.scm $(HOME)/.config/guix/channels.scm; \
	fi

remove-softlinks: ## Remove all softlinks
	@for file in $(CONFIG_FILES); do \
		if [ -h $(HOME)/$$file ]; then \
			rm -v $(HOME)/$$file; \
		fi \
	done
	@if [ -h $(HOME)/.bash_profile ]; then \
		rm -v $(HOME)/.bash_profile; \
	fi
	@if [ -h $(HOME)/.inputrc ]; then \
		rm -v $(HOME)/.inputrc; \
	fi
	@if [ -h $(HOME)/.config/guix/channels.scm ]; then \
		rm -v $(HOME)/.config/guix/channels.scm; \
	fi
