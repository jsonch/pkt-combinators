
`lib/compile.py` has the `compile_dpdk` function, which takes a pipe defined with the constructors in `lib/usersyntax.py` and compiles it to a dpdk program. 

example usage: `./parse_count_print.py` -- compile a program that extracts eth hdr, counts ip packets, and prints current count.

