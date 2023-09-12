SCRIPTS			:= $(HOME)/.local/scripts
MKDIR			:= mkdir -p
LN				:= ln -vsf
RM				:= rm -v
SUDO_LN			:= sudo -E ln -vsf
LNDIR			:= @ln -vsf
PKGINSTALL		:= sudo pacman --noconfirm --needed -S
AURINSTALL		:= paru --needed -S
PKGRM			:= sudo pacman -Rns
SRCDIR			:= $(HOME)/.local/src
MYGIT			:= git clone https://github.com/s4izh
SYSTEMD_ENABLE	:= sudo systemctl enable
BOOTSTRAP		:= @bash $(PWD)/archlinux/bootstrap

BASE_PKGS		:= filesystem gcc-libs glibc bash coreutils file findutils gawk grep procps-ng sed tar gettext
BASE_PKGS		+= pciutils psmisc shadow util-linux bzip2 gzip xz licenses pacman systemd systemd-sysvcompat
BASE_PKGS		+= iputils iproute2 autoconf sudo automake binutils bison fakeroot flex gcc groff libtool m4
BASE_PKGS		+= make patch pkgconf texinfo which usbutils

UTILS_PKGS		:= fzf direnv zip unzip neofetch tree wget jq

DESKTOP_PKGS	:= firefox discord network-manager-applet texlive zathura-pdf-poppler thunar pandoc-cli pandoc-crossref
DESKTOP_PKGS	+= mpv figlet pavucontrol xdg-utils xclip xsel xdotool xorg-xbacklight xorg-xrandr xorg-xsetroot redshift
DESKTOP_PKGS	+= xautolock yt-dlp xdg-desktop-portal-gtk xdg-user-dirs maim

AUR_PKGS		:= nwg-look-bin xdg-ninja

FONT_PKGS		:= ttf-liberation-mono-nerd

ZEN_PKGS		:= amd-ucode xf86-video-amdgpu xf86-video-ati brightnessctl
RX_PKGS			:= intel-ucode xf86-video-amdgpu vulkan-radeon lib32-vulkan-radeon

.DEFAULT_GOAL	:= help

.PHONY:			https-to-ssh pacmancolors fix-keys test

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| sort \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

backup: ## backup arch linux packages
	mkdir -p $(PWD)/archlinux
	pacman -Qqn > $(PWD)/archlinux/pacmanlist
	pacman -Qqem > $(PWD)/archlinux/aurlist

# pkgs
pkgs-base: ## base pkgs
	$(PKGINSTALL) $(BASE_PKGS)

pkgs-utils: ## package useful to have available, no desktop specific
	$(PKGINSTALL) $(UTILS_PKGS)

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
	# @if [ -h $(HOME)/.xinitrc ]; then $(RM) $(HOME)/.xinitrc; fi
	# $(LNDIR) $(PWD)/.xinitrc $(HOME)/.xinitrc

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

dwl:
	$(PKGINSTALL) wlroots wayland-protocols
	$(MYGIT)/$@ $(SRCDIR)/$@
	cd $(SRCDIR)/$@ && sudo make clean install
	sed -i 's#https://github.com/s4izh#git@github.com:s4izh#' "$(SRCDIR)/$@/.git/config"

foot:
	$(PKGINSTALL) foot
	@if [ -h $(HOME)/.config/$@ ]; then $(RM) $(HOME)/.config/$@; fi
	$(LNDIR) $(PWD)/.config/$@ $(HOME)/.config/$@

nvim:
	$(PKGINSTALL) neovim ripgrep
	$(MYGIT)/$@ $(HOME)/.config/$@
	sed -i 's#https://github.com/s4izh#git@github.com:s4izh#' "$(HOME)/.config/$@/.git/config"
	@if [ -h $(HOME)/.editorconfig ]; then $(RM) $(HOME)/.editorconfig; fi
	$(LNDIR) $(PWD)/.editorconfig $(HOME)/.editorconfig

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

xdg:
	$(PKGINSTALL) xdg-desktop-portal-gtk xdg-user-dirs xdg-utils
	@if [ -h $(HOME)/.config/user-dirs.dirs ]; then $(RM) $(HOME)/.config/user-dirs.dirs; fi
	@if [ -h $(HOME)/templates ]; then $(RM) $(HOME)/templates; fi
	$(LN) $(PWD)/.config/user-dirs.dirs $(HOME)/.local/user-dirs.dirs
	$(LNDIR) $(PWD)/templates $(HOME)/templates
	xdg-user-dirs-update

docker: ## docker setup and utils
	$(PKGINSTALL) $@ $@-compose $@-buildx
	sudo usermod -aG $@ ${USER}
	$(SYSTEMD_ENABLE) $@.service

scripts:
	@if [ -h $(HOME)/.local/scripts ]; then $(RM) $(HOME)/.local/scripts; fi
	$(LNDIR) $(PWD)/.local/scripts $(HOME)/.local/scripts

fontconfig:
	$(PKGINSTALL) $@
	@if [ -h $(HOME)/.config/$@ ]; then $(RM) $(HOME)/.config/$@; fi
	$(LNDIR) $(PWD)/.config/$@ $(HOME)/.config/$@

wget:
	$(PKGINSTALL) $@
	@if [ -h $(HOME)/.config/$@ ]; then $(RM) $(HOME)/.config/$@; fi
	$(LNDIR) $(PWD)/.config/$@ $(HOME)/.config/$@

gaming: ## gaming utils
	sudo pacman --needed -S steam lutris wine-mono

audio: ## audio utils
	$(PKGINSTALL) pipewire pipewire-pulse wireplumber

desktop: base audio
	$(PKGINSTALL) $(DESKTOP_PKGS)

suckless: dwm dwmblocks dmenu ## my suckless software forks (dwm, dwmblocks, dmenu)

dwm-deploy: dirs suckless x11 xinitrc sxhkd nvim alacritty tmux git paru xdg pkgs-desktop ## deploy all desktop with dwm

pacmancolors: ## enable pacman colors
	sudo sed -i "s/^#Color/Color/" /etc/pacman.conf

test-docker-image: docker
	docker build -t dotfiles ${PWD}

docker-target: dirs nvim tmux git paru pkgs-utils pacman-colors ## target for the docker image

test-target: dwm-deploy pacman-colors ## target for the test, run inside docker

test-docker-run: test-docker-image ## get a shell into the test docker image
	docker run -it --rm --name maketest -v $(PWD):$(PWD) \
                dotfiles:latest su -l sergio

test-docker-non-iteractive: test-docker-image ## test this Makefile with docker without backup directory
	docker run -it --rm --name maketest -v $(PWD):$(PWD) \
                dotfiles:latest sh -c "cd ${PWD}; make test-target"

https-to-ssh:
	sed -i 's#https://github.com/s4izh#git@github.com:s4izh#' "$(PWD)/$@/.git/config"

fix-keys: ## use when keys break
	sudo pacman-key --init && sudo pacman-key --populate archlinux && sudo pacman-key --refresh-keys
