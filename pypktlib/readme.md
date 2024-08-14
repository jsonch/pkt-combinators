
`lib/compile.py` has the `compile_dpdk` function, which takes a pipe defined with the constructors in `lib/usersyntax.py` and compiles it to a dpdk program. 

TODO: 
-  print out the code blocks of an atom
-  write a script to compile and run a dpdk app
1. add support for integer literals as atom arguments
2. add parametric atoms that can be initialized with different init function parameters
3. add a way to define user types
4. use ptr types consistently in backends
6. add a standard library of common atoms
7. test dpdk output
