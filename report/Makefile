out ?= $(PWD)/../site/

build:
	typst compile $(PWD)/report.typ --font-path $(PWD)/fonts/

install:
	cp $(PWD)/report.pdf $(out)/report.pdf
