
`lib/compile.py` has the `compile_dpdk` function, which takes a pipe defined with the constructors in `lib/usersyntax.py` and compiles it to a dpdk program. 

example usage: `./parse_count_print.py` -- compile a program that extracts an eth hdr, counts ip packets, and prints the current count.

### Code structure (2/2025)

**Examples**
- `apps/parse_count_print.py` is a minimal example. It should work.
- `apps/mac.py` and `project.py` are other examples that should compile to working dpdk-c.
  
**Compiler code**
- `compile.py` has the main compile function, `build(pipe, build_dir)`
- `usersyntax.py` has the user-facing syntax, the "smart constructors", and translation to the core AST.
    - `usersyntax.PipeBase` is the parent type of all source mario programs.
- `syntax.py` is the core AST.
- `frontend.py` has a bunch of normalization passes over the core AST (some of which can be removed due to various simplifications).
- `backend.py`  converts the core AST to a "segment" program. Each segment is a pipe that runs on a specific thread on a specific core. Segments are connected with single input single output queues.
- `dpdk_printer.py` prints the segment program to DPDK in a fairly direct manner.
