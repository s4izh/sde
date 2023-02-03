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

x11: xinitrc sxhkd
	$(LN) $(PWD)/.config/x11 $(HOME)/.config/x11

xinitrc:
	$(LN) $(PWD)/.xinitrc $(HOME)/.xinitrc
	$(LN) $(PWD)/.local/autostart.sh $(HOME)/.local/autostart.sh

sxhkd:
	$(LN) $(PWD)/.config/sxhkd $(HOME)/.config/sxhkd

shell:
	$(LN) $(PWD)/.config/shell $(HOME)/.config/shell
	$(LN) $(PWD)/.config/shell/profile $(HOME)/.zprofile
	$(LN) $(PWD)/.config/shell/profile $(HOME)/.profile

zsh: shell
	$(LN) $(PWD)/.config/zsh $(HOME)/.config/zsh

scripts:
	$(LN) $(PWD)/.local/scripts $(HOME)/.local/scripts

alacritty:
	$(LN) $(PWD)/.config/alacritty $(HOME)/.config/alacritty

nvim:
	$(LN) $(PWD)/.config/nvim $(HOME)/.config/nvim

tmux:
	$(LN) $(PWD)/.config/tmux $(HOME)/.config/tmux

zathura:
	$(LN) $(PWD)/.config/zathura $(HOME)/.config/zathura

picom:
	$(LN) $(PWD)/.config/picom $(HOME)/.config/picom

ranger:
	$(LN) $(PWD)/.config/ranger $(HOME)/.config/ranger

common: x11 zathura picom

init: xinitrc x11 shell nvim tmux zathura picom ranger ## deploy all dotfiles
