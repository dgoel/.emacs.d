emacs ?= emacs

BASEDIR := $(shell pwd)
BYTE_COMPILE_DIRS = etc/themes site-lisp defuns

install: ## Setup required directories and install system dependencies
	mkdir -p bin/irony
	sudo apt install ripgrep fd-find fzf
	sudo apt install \
		clang-format \
		clangd \
		elpa-org \
		jupyter
	pip3 install --upgrade \
		\'python-lsp-server[all]\' \
		mypy-ls \
		pyls-flake8 \
		pyls-isort \
		python-lsp-black \
		graphviz

pull: ## Update .emacs.d git repository and submodules
	git pull --recurse-submodules=yes

up: ## Update emacs packages installed through package manager
	-$(emacs) -batch -l packages.el

%.elc: %.el
	@echo Compiling file $<
	@$(emacs) -Q -batch  -f batch-byte-compile $<

compile: ## Byte compule elisp files in directories specified by ${BYTE_COMPILE_DIRS}
	for i in $(BYTE_COMPILE_DIRS); do \
		$(emacs) -Q -batch --eval '(batch-byte-recompile-directory 0)' $$i; \
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
	@IFS=$$'\n' ; \
	help_lines=(`fgrep -h "##" $(MAKEFILE_LIST) | fgrep -v fgrep | sed -e 's/\\$$//'`); \
	for help_line in $${help_lines[@]}; do \
		IFS=$$'#' ; \
		help_split=($$help_line) ; \
		help_command=`echo $${help_split[0]} | sed -e 's/^ *//' -e 's/ *$$//'` ; \
		help_info=`echo $${help_split[2]} | sed -e 's/^ *//' -e 's/ *$$//'` ; \
		printf "%-30s %s\n" $$help_command $$help_info ; \
	done


.PHONY: install pull up clean help
