emacs ?= emacs

BASEDIR := $(shell pwd)
BYTE_COMPILE_DIRS = etc/themes site-lisp defuns

profile:
	$(emacs) -Q -l profile-dotemacs.el \
	--eval "(setq profile-dotemacs-file \
        (setq load-file-name \"$(abspath init.el)\"))" \
	-f profile-dotemacs

pull:
	git pull
	git submodule init
	git submodule update

install: pull
	mkdir var
	$(emacs) -batch -l packages.el
	sudo dnf install emacs-auctex emacs-auctex-doc the_silver_searcher

compile:
	for i in $(BYTE_COMPILE_DIRS); do \
		$(emacs) -Q -batch --eval '(batch-byte-recompile-directory 0)' $$i; \
	done

up: pull
	$(emacs) -batch -l packages.el
	$(emacs) -Q -l init.el

run:
	$(emacs) -Q -l init.el

%.elc: %.el
	@echo Compiling file $<
	@$(emacs) -Q -batch  -f batch-byte-compile $<

clean:
	rm -f *.elc
	find . -name '*.elc' | while read file ; do \
            if ! test -f $$(echo $$file | sed 's/\.elc$$/.el/'); then \
                echo Removing old file: $$file ; \
                rm $$file ; \
            fi ; \
        done
.PHONY: profile pull install up run
