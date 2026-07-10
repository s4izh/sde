.DEFAULT_GOAL := help

EMACS_DIR := $(PWD)
MINIMAL_DIR := $(EMACS_DIR)/minimal

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| sort \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

submodules:
	git submodule update --init --recursive

deploy: deploy-minimal ## alias for deploy-minimal

deploy-minimal: submodules ## deploy the minimal-emacs.d based configuration
	@rm -vf $(HOME)/.emacs.d || exit 0
	@ln -svf $(MINIMAL_DIR)/minimal-emacs.d $(HOME)/.emacs.d
	@rm -vf $(HOME)/.emacs.d/post-init.el || exit 0
	@ln -svf $(MINIMAL_DIR)/post-init.el $(HOME)/.emacs.d/post-init.el
	@rm -vf $(HOME)/.emacs.d/lisp || exit 0
	@ln -svf $(MINIMAL_DIR)/lisp $(HOME)/.emacs.d/lisp

deploy-simple: ## deploy the simple (vanilla-first) configuration
	@rm -vf $(HOME)/.emacs.d || exit 0
	@ln -svf $(EMACS_DIR)/simple $(HOME)/.emacs.d

deploy-old: ## deploy the old configuration
	@rm -vf $(HOME)/.emacs.d || exit 0
	@ln -svf $(EMACS_DIR)/old $(HOME)/.emacs.d
