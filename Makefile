.DEFAULT_GOAL := help

SRC_DIR := $(HOME)/.local/src

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| sort \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

repl: ## Start a nix repl with the flake loaded
	nix repl --expr 'builtins.getFlake (toString ./.)'
test: ## Test the flake
	sudo nixos-rebuild test --flake .# --impure
switch: src-dependencies ## Switch to the flake
	sudo nixos-rebuild switch --flake .# --impure
update: ## Update the flake
	nix flake update
user: ## Switch to the flake as a user
	nix build ".#homeManagerConfigurations.sergio.activationPackage" --impure
	./result/activate
	rm -rf result
src-dependencies: ## clone src repos
	if [ ! -d $(SRC_DIR) ]; then mkdir -p $(SRC_DIR); fi
	if [ ! -d $(SRC_DIR)/dwm ]; then git clone https://github.com/s4izh/dwm.git $(SRC_DIR)/dwm; fi
	if [ ! -d $(SRC_DIR)/dmenu ]; then git clone https://github.com/s4izh/dmenu.git $(SRC_DIR)/dmenu; fi
	if [ ! -d $(SRC_DIR)/dwmblocks ]; then git clone https://github.com/s4izh/dwmblocks.git $(SRC_DIR)/dwmblocks; fi
