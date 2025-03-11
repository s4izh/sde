.DEFAULT_GOAL := help

.PHONY: src-dependencies create-softlinks remove-softlinks

-include config.mk

ifeq ($(SDE_DIR),)
  $(error SDE_DIR is not set. Please define it before running this Makefile.)
endif

ifeq ($(LINKS_FROM_SDE_TO_HOME),)
  $(error LINKS_FROM_SDE_TO_HOME is not set. Please define it before running this Makefile.)
endif

ifeq ($(DIRS_TO_CREATE_IN_HOME),)
  $(error DIRS_TO_CREATE_IN_HOME is not set. Please define it before running this Makefile.)
endif

ifeq ($(GIT_SRCS),)
  $(error GIT_SRCS is not set. Please define it before running this Makefile.)
endif

# REPO_NAMES := $(notdir $(basename $(GIT_SRCS)))

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(filter-out config.mk, $(MAKEFILE_LIST)) \
	| sort \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

repl: ## Start a nix repl with the flake loaded
	nix repl --expr 'builtins.getFlake (toString ./.)'

test: ## Test the flake
	sudo nixos-rebuild test --flake .# --impure

switch: src-dependencies ## Switch to the flake
	# sudo nixos-rebuild switch --flake .\?submodules=1# --impure 
	nh os switch --ask .\?submodules=1 -- --impure

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

create-dirs: ## Create expected directories
	@for dir in $(DIRS); do \
		mkdir -pv $$dir; \
	done

create-softlinks: remove-softlinks create-dirs ## Create all softlinks
	@for pair in $(LINKS_FROM_SDE_TO_HOME); do \
		src=$${pair%%:*}; \
		dest=$${pair##*:}; \
		ln -svf $(SDE_DIR)/$$src $(HOME)/$$dest; \
	done

remove-softlinks: ## Remove all softlinks
	@for pair in $(LINKS_FROM_SDE_TO_HOME); do \
		dest=$${pair##*:}; \
		if [ -h $(HOME)/$$dest ]; then \
			rm -v $(HOME)/$$dest; \
		fi; \
	done
