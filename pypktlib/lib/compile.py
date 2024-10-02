"""toplevel compilation functions"""
import sys
import os
from .usersyntax import *
# import syntax
from . import frontend
from . import backend

def compile_dpdk(pipe:PipeBase):
    """Compile a pipe defined with the usersyntax constructors to DPDK code."""
    ir_pipe = pipe.to_ir()
    pipe_prog = frontend.frontend_passes(ir_pipe)
    segment_prog = backend.backend_passes(pipe_prog)
    return backend.irprog_to_dpdkcode(segment_prog)


def build(pipe, build_dir):
    """Build a pipe and write it to the given directory"""
    # make the directory
    print("Compiling pipe to directory:", build_dir)
    os.makedirs(build_dir, exist_ok=True)
    # build and write the program to the c file
    prog_str = compile_dpdk(pipe)
    out_fn = os.path.join(build_dir, "pipe.c")
    with open(out_fn, 'w') as f:
        f.write(prog_str)
    # add the makefile and run script
    backend.copy_dpdk_resources(build_dir)
    