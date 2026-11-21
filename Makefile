emacs ?= emacs

BASEDIR := $(shell pwd)
BYTE_COMPILE_DIRS = site-lisp themes defuns etc

install: ## Setup required directories and install system dependencies
	mkdir -p bin var
	sudo apt install ripgrep fd-find fzf
	sudo apt install \
		aspell \
		clang-format \
		clangd \
		golang-go

up: ## Install, upgrade and prune packages to match etc/01-packages.el
	$(emacs) -Q -batch -l packages.el
	@$(MAKE) --no-print-directory precompile

precompile: ## Native-compile packages now, instead of lazily while you edit
	$(emacs) -Q -batch -l native.el

grammars: ## Install missing tree-sitter grammars from treesit-language-source-alist
	$(emacs) -Q -batch -l grammars.el

audit: ## Report drift between etc/01-packages.el and the use-package forms
	@$(emacs) -Q -batch -l audit.el

pull: ## Update the .emacs.d git repository
	git pull

%.elc: %.el
	@echo Compiling file $<
	@$(emacs) -Q -batch -f batch-byte-compile $<

compile: ## Byte compile init.el and elisp files in ${BYTE_COMPILE_DIRS}
	$(emacs) -Q -batch --eval '(progn (setq package-directory-list nil) (add-to-list (quote load-path) (expand-file-name "site-lisp")) (package-initialize) (batch-byte-compile))' init.el
	for i in $(BYTE_COMPILE_DIRS); do \
		$(emacs) -Q -batch --eval '(progn (setq package-directory-list nil) (add-to-list (quote load-path) (expand-file-name "site-lisp")) (package-initialize) (batch-byte-recompile-directory 0))' $$i; \
	done

clean: ## Cleanup generated elc files
	rm -f *.elc
	find . -name '*.elc' | while read file ; do \
            if ! test -f $$(echo $$file | sed 's/\.elc$$/.el/'); then \
                echo Removing old file: $$file ; \
                rm $$file ; \
            fi ; \
        done

help: ## This help dialog.
	@grep -hE '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
		| awk 'BEGIN {FS = ":.*?## "}; {printf "%-20s %s\n", $$1, $$2}'

.PHONY: install up precompile grammars audit pull compile clean help
