ROOT_DIR := $(shell while [ "$$PWD" != "/" ] && [ ! -f dune-project ]; do cd ..; done; pwd)

build:
	rm -f main
	dune build --root=$(ROOT_DIR)
	cp $(ROOT_DIR)/_build/default/progs/prog1/main.exe ./main	