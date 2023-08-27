SCRIPTS			:= $(HOME)/.local/scripts
MKDIR			:= mkdir -p
LN				:= ln -vsf
RM				:= rm -v
SUDO_LN			:= sudo -E ln -vsf
LNDIR			:= @ln -vsf
PKGINSTALL		:= sudo pacman --noconfirm --needed -S
AURINSTALL		:= paru --noconfirm --needed -S
PKGRM			:= sudo pacman -Rns
SRCDIR			:= $(HOME)/.local/src
MYGIT			:= git clone https://github.com/s4izh
SYSTEMD_ENABLE	:= sudo systemctl --now enable
BOOTSTRAP		:= @bash $(PWD)/archlinux/bootstrap

BASE_PKGS		:= filesystem gcc-libs glibc bash coreutils file findutils gawk grep procps-ng sed tar gettext
BASE_PKGS		+= pciutils psmisc shadow util-linux bzip2 gzip xz licenses pacman systemd systemd-sysvcompat
BASE_PKGS		+= iputils iproute2 autoconf sudo automake binutils bison fakeroot flex gcc groff libtool m4
BASE_PKGS		+= make patch pkgconf texinfo which

PKGS			:= fzf direnv
DESKTOP_PKGS	:= firefox
#AUR_PKGS		:=


.DEFAULT_GOAL	:= help

.PHONY:			https-to-ssh pacmancolors fix-keys test

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| sort \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

backup: ## backup arch linux packages
	mkdir -p $(PWD)/archlinux
	pacman -Qqen > $(PWD)/archlinux/pacmanlist
	pacman -Qqem > $(PWD)/archlinux/aurlist

# pkgs
pkgs-base: ## base pkgs
	$(PKGINSTALL) $(BASE_PKGS)

pkgs-desktop: ## desktop pkgs
	$(PKGINSTALL) $(DESKTOP_PKGS)

pkgs-aur: ## aur packages
	$(PKGINSTALL) $(AUR_PKGS)

network-manager: ## install and enable network manager
	$(PKGINSTALL) networkmanager
	$(SYSTEMD_ENABLE) NetworkManager.service

dirs: ## create needed dirs
	@mkdir -p $(HOME)/.local/src
	@mkdir -p $(HOME)/.local/share
	@mkdir -p $(HOME)/.config

shell:
	@if [ -h $(HOME)/.config/$@ ]; then $(RM) $(HOME)/.config/$@; fi
	$(LNDIR) $(PWD)/.config/$@ $(HOME)/.config/$@
	cp -dfv $(PWD)/.zprofile $(HOME)/.zprofile
	cp -dfv $(PWD)/.zprofile $(HOME)/.profile

zsh: ## install zsh and change to zsh
	$(BOOTSTRAP)/zsh $(USER) $(PWD)

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

libvirtd: audio ## virtualisation utils
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

scripts:
	@if [ -h $(HOME)/.local/scripts ]; then $(RM) $(HOME)/.local/scripts; fi
	$(LNDIR) $(PWD)/.local/scripts $(HOME)/.local/scripts

audio:
	$(PKGINSTALL) pipewire pipewire-pulse wireplumber

desktop: base audio
	$(PKGINSTALL) $(DESKTOP_PKGS)

suckless: dwm dwmblocks dmenu ## my suckless software forks (dwm, dwmblocks, dmenu)

dwm-deploy: dirs suckless x11 xinitrc sxhkd nvim alacritty tmux git paru pkgs-desktop ## deploy all desktop with dwm

pacmancolors:
	sudo sed -i "s/^#Color/Color/" /etc/pacman.conf

test-docker-image: docker
	docker build -t dotfiles ${PWD}

test-target: dwm-deploy libvirtd docker pacman-colors

test-docker-run: test-docker-image
	docker run -it --rm --name maketest -v $(pwd):$(pwd) \
                dotfiles:latest su -l sergio

test: docker-image ## test this Makefile with docker without backup directory
	docker run -it --rm --name make$@ -v $(PWD):$(PWD) \
		-d dotfiles:latest /bin/bash
	docker exec -it make$@ sh -c "cd ${PWD}; make test-target"

https-to-ssh:
	sed -i 's#https://github.com/s4izh#git@github.com:s4izh#' "$(PWD)/$@/.git/config"

fix-keys: ## use when keys break
	sudo pacman-key --init && sudo pacman-key --populate archlinux && sudo pacman-key --refresh-keys
