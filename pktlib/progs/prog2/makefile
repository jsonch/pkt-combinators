ROOT_DIR := $(shell while [ "$$PWD" != "/" ] && [ ! -f dune-project ]; do cd ..; done; pwd)

build:
	rm -f main
	dune build --root=$(ROOT_DIR)
	cp $(ROOT_DIR)/_build/default/progs/prog2/main.exe ./main
	cp $(ROOT_DIR)/../examples/portknock_c2/portknock ./portknock