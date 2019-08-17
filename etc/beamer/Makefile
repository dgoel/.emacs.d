MAKEFLAGS  := -j 1
DEMO_SRC    = demo/demo.tex demo/demo.bib
DEMO_PDF    = demo/demo.pdf

# presentation
PRESENTATION_SRC=presentation.tex
PRESENTATION_PDF=presentation.pdf

DESTDIR     ?= $(shell kpsewhich -var-value=TEXMFHOME)
INSTALL_DIR  = $(DESTDIR)/tex/latex/metropolis
DOC_DIR      = $(DESTDIR)/doc/latex/metropolis
CACHE_DIR   := $(shell pwd)/.latex-cache

COMPILE_TEX := latexmk -xelatex -output-directory=$(CACHE_DIR)
export TEXINPUTS:=$(shell pwd):$(shell pwd)/source:${TEXINPUTS}

DOCKER_IMAGE = latex-image
DOCKER_CONTAINER = latex-container

.PHONY: all demo clean clean-cache docker-run docker-build docker-rm

all: presentation

demo: $(DEMO_PDF)

clean: clean-cache
	@rm -f "$(PRESENTATION_PDF)"

clean-cache:
	@rm -rf "$(CACHE_DIR)"

$(CACHE_DIR):
	@mkdir -p $(CACHE_DIR)

$(DEMO_PDF): $(DEMO_SRC) | clean-cache $(CACHE_DIR)
	@cd $(dir $(DEMO_SRC)) && $(COMPILE_TEX) $(notdir $(DEMO_SRC))
	@cp $(CACHE_DIR)/$(notdir $(DEMO_PDF)) $(DEMO_PDF)

presentation: $(PRESENTATION_SRC) | clean-cache $(CACHE_DIR)
	$(COMPILE_TEX) $(notdir $(PRESENTATION_SRC))
	@cp $(CACHE_DIR)/$(notdir $(PRESENTATION_PDF)) $(PRESENTATION_PDF)

docker-run: docker-build
	docker run --rm=true --name $(DOCKER_CONTAINER) -i -t -v `pwd`:/data $(DOCKER_IMAGE) make

docker-build:
	docker build -t $(DOCKER_IMAGE) docker

docker-rm:
	docker rm $(DOCKER_CONTAINER)
