.DEFAULT_GOAL := help

MODULES=modules
HOSTNAME ?= $(shell hostname)

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

repl: ## run a repl with modules on load path
	guix repl -L $(MODULES)

authorize-nonguix: ## usefull for first deploy
	sudo guix archive --authorize < substitutes/nonguix.pub
