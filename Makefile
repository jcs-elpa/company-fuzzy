SHELL := /usr/bin/env bash

EMACS ?= emacs
CASK ?= cask

PKG-FILES := company-fuzzy.el

TEST-FILES := $(shell ls test/company-fuzzy-*.el)

.PHONY: clean checkdoc lint build compile unix-test

ci: clean build compile

build:
	EMACS=$(EMACS) $(CASK) install
	EMACS=$(EMACS) $(CASK) build

compile:
	@echo "Compiling..."
	@$(EMACS) -nw --batch \
		-L . \
		--eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile $(PKG-FILES)

unix-test:
	@echo "Testing..."
	$(CASK) exec ert-runner -L . $(LOAD-TEST-FILES) -t '!no-win' -t '!org'

clean:
	rm -rf .cask *.elc
