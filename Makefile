BASE=$(PWD)
SCRIPTS=$(HOME)/.local/scripts
MKDIR=mkdir -p
LN=ln -vsf
RM=rm -v
SUDO_LN=sudo -E ln -sf
LNDIR=@ln -vsf
PKGINSTALL=sudo pacman --noconfirm --needed -S
SRCDIR=$(HOME)/.local/src
DOTDIR=$(HOME)/.dotfiles
BOOTSTRAP=@bash $(DOTDIR)/archlinux/bootstrap
MYGIT=git clone https://github.com/s4izh
SYSTEMD_ENABLE	:= sudo systemctl --now enable

.DEFAULT_GOAL := help

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| sort \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

backup: ## backup arch linux packages
	mkdir -p $(DOTDIR)/archlinux
	pacman -Qqen > $(DOTDIR)/archlinux/pacmanlist
	pacman -Qqem > $(DOTDIR)/archlinux/aurlist

dirs: ## create needed dirs
	@mkdir -p $(HOME)/.local/src
	@mkdir -p $(HOME)/.local/share
	@mkdir -p $(HOME)/.config

shell:
	@if [ -h $(HOME)/.config/$@ ]; then $(RM) $(HOME)/.config/$@; fi
	$(LNDIR) $(DOTDIR)/.config/$@ $(HOME)/.config/$@
	cp -dfv $(DOTDIR)/.zprofile $(HOME)/.zprofile
	cp -dfv $(DOTDIR)/.zprofile $(HOME)/.profile

zsh: shell ## install zsh and change to zsh
	$(BOOTSTRAP)/zsh

paru: ## install paru
	$(BOOTSTRAP)/paru

x11:
	$(PKGINSTALL) xorg-server
	@if [ -h $(HOME)/.config/$@ ]; then $(RM) $(HOME)/.config/$@; fi
	$(LNDIR) $(PWD)/.config/$@ $(HOME)/.config/$@

xinitrc:
	$(PKGINSTALL) xorg-xinit
	@if [ -h $(HOME)/.xinitrc ]; then $(RM) $(HOME)/.xinitrc; fi
	$(LNDIR) $(PWD)/.xinitrc $(HOME)/.xinitrc

sxhkd:
	$(PKGINSTALL) sxhkd
	@if [ -h $(HOME)/.config/$@ ]; then $(RM) $(HOME)/.config/$@; fi
	$(LNDIR) $(PWD)/.config/$@ $(HOME)/.config/$@

dwm:
	$(PKGINSTALL) freetype2 libx11 libxft libxinerama
	$(MYGIT)/$@ $(SRCDIR)/$@
	cd $(SRCDIR)/$@ && sudo make clean install
	sed -i 's#https://github.com/s4izh#git@github.com:s4izh#' "$(SRCDIR)/$@/.git/config"

dwmblocks:
	$(MYGIT)/$@ $(SRCDIR)/$@
	cd $(SRCDIR)/$@ && sudo make clean install
	sed -i 's#https://github.com/s4izh#git@github.com:s4izh#' "$(SRCDIR)/$@/.git/config"

dmenu:
	$(PKGINSTALL) coreutils fontconfig freetype2 glibc fontconfig libx11 libxft libxinerama
	$(MYGIT)/$@ $(SRCDIR)/$@
	cd $(SRCDIR)/$@ && sudo make clean install
	sed -i 's#https://github.com/s4izh#git@github.com:s4izh#' "$(SRCDIR)/$@/.git/config"

st:
	$(PKGINSTALL) freetype2 libxft harfbuzz
	$(MYGIT)/$@ $(SRCDIR)/$@
	cd $(SRCDIR)/$@ && sudo make clean install
	sed -i 's#https://github.com/s4izh#git@github.com:s4izh#' "$(SRCDIR)/$@/.git/config"

nvim:
	$(PKGINSTALL) neovim ripgrep
	$(MYGIT)/$@ $(HOME)/.config/$@
	sed -i 's#https://github.com/s4izh#git@github.com:s4izh#' "$(HOME)/.config/$@/.git/config"

alacritty:
	$(PKGINSTALL) alacritty
	@if [ -h $(HOME)/.config/$@ ]; then $(RM) $(HOME)/.config/$@; fi
	$(LNDIR) $(PWD)/.config/$@ $(HOME)/.config/$@

tmux:
	$(PKGINSTALL) tmux
	@if [ -h $(HOME)/.config/$@ ]; then $(RM) $(HOME)/.config/$@; fi
	$(LNDIR) $(PWD)/.config/$@ $(HOME)/.config/$@

git:
	@if [ -h $(HOME)/.config/$@ ]; then $(RM) $(HOME)/.config/$@; fi
	$(LNDIR) $(PWD)/.config/git $(HOME)/.config/git

share:
	@if [ -h $(HOME)/.local/share/sergio ]; then $(RM) $(HOME)/.local/share/sergio; fi
	$(LNDIR) $(PWD)/.local/share/sergio $(HOME)/.local/share/sergio

libvirtd: ## virtualisation utils
	sudo pacman --needed -S qemu-full bridge-utils libvirt virt-manager \
        dhclient openbsd-netcat dnsmasq dmidecode ebtables \
        bridge-utils iptables-nft virt-install virt-manager virt-viewer
	@if ! [ -d $(HOME)/.config/libvirt ]; then mkdir $(HOME)/.config/libvirt; fi
	@if [ -h $(HOME)/.config/libvirt/libvirt.conf ]; then $(RM) $(HOME)/.config/libvirt/libvirt.conf; fi
	$(LN) $(PWD)/.config/libvirt/libvirt.conf $(HOME)/.config/libvirt/libvirt.conf
	sudo usermod -aG libvirt $(USER)
	$(SYSTEMD_ENABLE) $@.service

docker: ## docker initial setup
	$(PKGINSTALL) $@ $@-compose $@-buildx
	sudo usermod -aG $@ ${USER}
	$(SYSTEMD_ENABLE) $@.service

suckless: dwm dwmblocks dmenu ## my suckless software forks (dwm, dwmblocks, dmenu)

dwm-deploy: dirs zsh suckless x11 xinitrc sxhkd nvim alacritty tmux git paru ## deploy all desktop with dwm

.PHONY: https-to-ssh
https-to-ssh:
	sed -i 's#https://github.com/s4izh#git@github.com:s4izh#' "$(DOTDIR)/$@/.git/config"

.PHONY: fix-keys
fix-keys: ## use when keys break
	sudo pacman-key --init && sudo pacman-key --populate archlinux && sudo pacman-key --refresh-keys

pacmancolors:
	sudo sed -i "s/^#Color/Color/" /etc/pacman.conf

docker_image: docker
	docker build -t dotfiles ${PWD}

.ONESHELL:
test: docker_image ## test this Makefile with docker without backup directory
	docker run -it --name make$@ -d dotfiles:latest /bin/bash
	for target in dwm-deploy libvirt docker; do
		docker exec -it make$@ sh -c "cd ${PWD}; make $${target}"
	done

