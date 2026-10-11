.DEFAULT_GOAL := help

EMACS_DIR := $(PWD)

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| sort \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

deploy: deploy-simple ## alias for deploy-simple

deploy-simple: ## deploy the simple (vanilla-first) configuration
	@rm -vf $(HOME)/.emacs.d || exit 0
	@ln -svf $(EMACS_DIR)/simple $(HOME)/.emacs.d
