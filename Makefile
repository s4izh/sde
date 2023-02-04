BASE=$(PWD)
SCRIPTS=$(HOME)/.local/scripts
MKDIR=mkdir -p
LN=ln -vsf
LNDIR=ln -vs
PKGINSTALL=sudo pacman --noconfirm -S

.DEFAULT_GOAL := help

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| sort \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

backup: ## Backup arch linux packages
	mkdir -p ${PWD}/archlinux
	pacman -Qnq > ${PWD}/archlinux/pacmanlist
	pacman -Qqem > ${PWD}/archlinux/aurlist

tlp: ## tlp package for better power management
	sudo pacman -S tlp powertop
# sudo $(LN) $(PWD)/etc/default/tlp /etc/default/tlp
	systemctl enable tlp.service
	systemctl enable tlp-sleep.service

x11: xinitrc sxhkd ## xasdf
	rm -f $(XDG_CONFIG_HOME)/x11
	$(LN) $(PWD)/.config/x11 $(HOME)/.config/x11

xinitrc: ## xinitrc
	rm -f $(HOME)/.xinitrc $(HOME)/.local/x_autostart.sh
	$(LN) $(PWD)/.xinitrc $(HOME)/.xinitrc
	$(LN) $(PWD)/.local/x_autostart.sh $(HOME)/.local/x_autostart.sh

sxhkd:
	rm -f $(XDG_CONFIG_HOME)/sxhkd
	$(LN) $(PWD)/.config/sxhkd $(HOME)/.config/sxhkd

shell:
	rm -f $(XDG_CONFIG_HOME)/shell $(HOME)/.zprofile $(HOME)/.profile
	$(LN) $(PWD)/.config/shell $(HOME)/.config/shell
	$(LN) $(HOME)/.config/shell/profile $(HOME)/.zprofile
	$(LN) $(HOME)/.config/shell/profile $(HOME)/.profile

zsh: shell
	rm -f $(XDG_CONFIG_HOME)/zsh
	$(LN) $(PWD)/.config/zsh $(HOME)/.config/zsh

scripts:
	rm -f $(HOME)/.local/scripts
	$(LN) $(PWD)/.local/scripts $(HOME)/.local/scripts

dwm:
	git clone git@github.com:s4izh/dwm.git ~/.local/src/dwm
	cd ~/.local/src/dwm
	sudo make clean install

alacritty:
	rm -f $(XDG_CONFIG_HOME)/alacritty
	$(LN) $(PWD)/.config/alacritty $(XDG_CONFIG_HOME)/alacritty

nvim:
	rm -f $(XDG_CONFIG_HOME)/nvim
	$(LN) $(PWD)/.config/nvim $(HOME)/.config/nvim

tmux:
	rm -f $(XDG_CONFIG_HOME)/tmux
	$(LN) $(PWD)/.config/tmux $(HOME)/.config/tmux

zathura:
	rm -f $(XDG_CONFIG_HOME)/zathura
	$(LN) $(PWD)/.config/zathura $(HOME)/.config/zathura

picom:
	rm -f $(XDG_CONFIG_HOME)/picom
	$(LN) $(PWD)/.config/picom $(HOME)/.config/picom

ranger:
	rm -f $(XDG_CONFIG_HOME)/ranger
	$(LN) $(PWD)/.config/ranger $(HOME)/.config/ranger

portatil: alacritty
	$(LN) $(XDG_CONFIG_HOME)/alacritty/fonts/laptop.yml $(XDG_CONFIG_HOME)/alacritty/font.yml

pc: alacritty
	$(LN) $(XDG_CONFIG_HOME)/alacritty/fonts/pc.yml $(XDG_CONFIG_HOME)/alacritty/font.yml

fzf:

server: ## setup server dotfiles, zsh, vimrc
	$(LN) $(PWD)/.vimrc $(HOME)/.vimrc

deploy: x11 zsh scripts alacritty nvim tmux zathura picom ranger
