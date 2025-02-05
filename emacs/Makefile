.DEFAULT_GOAL := help

EMACS_DIR := $(PWD)

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| sort \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

submodules:
	git submodule update --init --recursive

deploy: submodules ## deploy emacs configuration
	@rm -vf $(HOME)/.emacs.d || exit 0
	@ln -svf $(EMACS_DIR)/minimal-emacs.d $(HOME)/.emacs.d
	@rm -vf $(HOME)/.emacs.d/post-init.el || exit 0
	@ln -svf $(EMACS_DIR)/post-init.el $(HOME)/.emacs.d/post-init.el
	@rm -vf $(HOME)/.emacs.d/lisp || exit 0
	@ln -svf $(EMACS_DIR)/lisp $(HOME)/.emacs.d/lisp
