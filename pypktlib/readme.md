
`lib/compile.py` has the `compile_dpdk` function, which takes a pipe defined with the constructors in `lib/usersyntax.py` and compiles it to a dpdk program. 

example usage: `./parse_count_print.py` -- compile a program that extracts eth hdr, counts ip packets, and prints current count.

TODO: 
x -  print out the code blocks of an atom
x -  write a script to compile and run a dpdk app
x 1. add support for integer literals as atom arguments
x 2. add parametric atoms that can be initialized with different init function parameters
x 3. add a way to define user types
x 4. use ptr types consistently in backends
x 6. add a standard library of common atoms
7. test dpdk output
