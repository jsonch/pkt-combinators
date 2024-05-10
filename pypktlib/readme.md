
`mario.py` constructs a dpdk pipeline from a pipeline specification. 
The pipeline specification is written in python, using the library in `libs/syntax.py`. 
The pipeline specification should be inlined into the source c file, in `PYTHON({...})`
blocks, as shown in `progs/dpdk1.c`.

Example usage: 
1. run `./mario.py ./progs/dpdk1.c -o dpdk1_build`
2. `dpdk1_build/dpdk1_mario.c` is the compiled file
3. if you have dpdk installed, you should be able to run `make` in `dpdk1_build` to build the program, and `./pcaprun.sh` to run it on the test pcap
4. if you _don't_ have dpdk installed, follow `multipass_setup.md` in the toplevel of this repo for instructions on how to set up a multipass vm with dpdk installed.
