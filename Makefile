BASE=$(PWD)
SCRIPTS=$(HOME)/.local/scripts
MKDIR=mkdir -p
LN=ln -vsf
SUDO_LN=sudo -E ln -sf
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
	sudo $(LN) $(PWD)/etc/tlp.conf /etc/tlp.conf
	systemctl enable tlp.service
	systemctl enable tlp-sleep.service

x11: xinitrc sxhkd ## necessary x11 settings, includes xinitrc and sxhkd
	rm -f $(XDG_CONFIG_HOME)/x11
	$(LNDIR) $(PWD)/.config/x11 $(HOME)/.config/x11

xinitrc: ## xinitrc
	rm -f $(HOME)/.xinitrc $(HOME)/.local/x_autostart.sh
	$(LNDIR) $(PWD)/.xinitrc $(HOME)/.xinitrc
	$(LNDIR) $(PWD)/.local/x_autostart.sh $(HOME)/.local/x_autostart.sh

sxhkd:
	rm -f $(XDG_CONFIG_HOME)/sxhkd
	$(LNDIR) $(PWD)/.config/sxhkd $(HOME)/.config/sxhkd

shell:
	rm -f $(XDG_CONFIG_HOME)/shell $(HOME)/.zprofile $(HOME)/.profile
	$(LNDIR) $(PWD)/.config/shell $(HOME)/.config/shell
	$(LN) $(HOME)/.config/shell/profile $(HOME)/.zprofile
	$(LN) $(HOME)/.config/shell/profile $(HOME)/.profile

zsh: shell
	rm -f $(XDG_CONFIG_HOME)/zsh
	mkdir $(XDG_CACHE_HOME)/zsh
	$(LNDIR) $(PWD)/.config/zsh $(HOME)/.config/zsh

share:
	$(LNDIR) $(PWD)/.local/share/sergio $(HOME)/.local/share/sergio

scripts: share
	rm -f $(HOME)/.local/scripts
	$(LNDIR) $(PWD)/.local/scripts $(HOME)/.local/scripts

dwm:
	git clone git@github.com:s4izh/dwm.git ~/.local/src/dwm
	cd ~/.local/src/dwm && ln -sf themes/default.h theme.h
	cd ~/.local/src/dwm && sudo make clean install

dwmblocks:
	git clone git@github.com:s4izh/dwmblocks.git ~/.local/src/dwmblocks
	cd ~/.local/src/dwmblocks && sudo make clean install

dmenu:
	git clone git@github.com:s4izh/dmenu.git ~/.local/src/dmenu
	cd ~/.local/src/dmenu && ln -sf themes/default.h theme.h
	cd ~/.local/src/dmenu && sudo make clean install

st:
	git clone git@github.com:s4izh/st.git ~/.local/src/st
	cd ~/.local/src/st && ln -sf themes/default.h theme.h
	cd ~/.local/src/st && sudo make clean install

alacritty:
	rm -f $(XDG_CONFIG_HOME)/alacritty
	$(LNDIR) $(PWD)/.config/alacritty $(XDG_CONFIG_HOME)/alacritty

foot:
		# rm -f $(XDG_CONFIG_HOME)/foot
	$(LNDIR) $(PWD)/.config/foot $(XDG_CONFIG_HOME)/foot

sway:
	$(LNDIR) $(PWD)/.config/sway $(XDG_CONFIG_HOME)/sway

nvim:
	rm -f $(XDG_CONFIG_HOME)/nvim
	$(LNDIR) $(PWD)/.config/nvim $(HOME)/.config/nvim

tmux:
	rm -f $(XDG_CONFIG_HOME)/tmux
	$(LNDIR) $(PWD)/.config/tmux $(HOME)/.config/tmux

git:
	$(LNDIR) $(PWD)/.config/git $(HOME)/.config/git

zathura:
	rm -f $(XDG_CONFIG_HOME)/zathura
	$(LNDIR) $(PWD)/.config/zathura $(HOME)/.config/zathura

picom:
	rm -f $(XDG_CONFIG_HOME)/picom
	$(LNDIR) $(PWD)/.config/picom $(HOME)/.config/picom

ranger:
	rm -f $(XDG_CONFIG_HOME)/ranger
	$(LNDIR) $(PWD)/.config/ranger $(HOME)/.config/ranger

libvirt:
	rm -f $(XDG_CONFIG_HOME)/libvirt/libvirt.conf
	$(LN) $(PWD)/.config/libvirt/libvirt.conf $(XDG_CONFIG_HOME)/libvirt/libvirt.conf

mime:
	$(LN) $(PWD)/.config/mimeapps.list $(XDG_CONFIG_HOME)/mimeapps.list
xdg-user-dirs:
	$(LN) $(PWD)/.config/user-dirs.dirs $(XDG_CONFIG_HOME)/user-dirs.dirs

portatil: alacritty
	$(LN) $(XDG_CONFIG_HOME)/alacritty/fonts/laptop.yml $(XDG_CONFIG_HOME)/alacritty/font.yml

pc: alacritty
	$(LN) $(XDG_CONFIG_HOME)/alacritty/fonts/pc.yml $(XDG_CONFIG_HOME)/alacritty/font.yml

fzf:

server: ## setup server dotfiles, zsh, vimrc
	$(LN) $(PWD)/.vimrc $(HOME)/.vimrc

disp:
	$(SUDO_LN) $(PWD)/etc/X11/xorg.conf.d/00-keyboard.conf /etc/X11/xorg.conf.d/00-keyboard.conf
	$(SUDO_LN) $(PWD)/etc/X11/xorg.conf.d/10-touchpad.conf /etc/X11/xorg.conf.d/10-touchpad.conf
	$(SUDO_LN) $(PWD)/etc/X11/xorg.conf.d/50-mouse-acceleration.conf /etc/X11/xorg.conf.d/50-mouse-acceleration.conf

deploy: x11 zsh scripts alacritty nvim tmux zathura picom ranger
