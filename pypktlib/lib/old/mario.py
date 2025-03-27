#! /usr/bin/env python3
# minimal python prototype of combinator compiler
from dataclasses import dataclass
import re, os, sys, io, contextlib, argparse

from lib.syntax import *
from lib.frontend import *
from lib.backend import *

from lib.bpf_opt import *

usage = """
Mario: A packet pipeline compiler
Usage: mario.py <source.c> [-o <output_dir>]

This script will construct a packet processing pipeline defined in the 
PYTHON({...}) blocks of the source.c and generate a new c file with the 
contents of that block replaced by the generated code. The file will be 
written along with some helper files, to the output directory 
which defaults to the name of the source file with "_build" appended.
"""

def compile_dpdk(pipe:PipeBase):
    pipe_prog = frontend_passes(pipe)
    segment_prog = backend_passes(pipe_prog)
    transform(segment_prog.segments[1], segment_prog, "c0")
    return irprog_to_dpdkcode(segment_prog)

### run python in PYTHON blocks of a c file
def preprocess(src_fn: str):
    """
    Execute the python code in the PYTHON blocks of a c file. 
    All outputs printed to stdout in a PYTHON block are captured and 
    put into the source file at the location of that block. 
    """
    # Rest of the preprocessing logic goes here
    with open(src_fn, 'r') as f:
        src = f.read()
    def repl_python(match):
        code = match.group(1)
        # print(f"Running python code: {code}")
        # redirect standard out to a buffer, so that 
        # all the print statements in the python code are captured 
        # to be put back into the source file
        buffer = io.StringIO()
        with contextlib.redirect_stdout(buffer):
            exec(code, globals())
        # print(f"Output: {buffer.getvalue()}")
        orig_str = "/* PYTHON({"+code+"}) */"
        if buffer.getvalue() == "":
            return orig_str
        else:
            return orig_str + "\n//GENERATED CODE:\n" + buffer.getvalue()        
    dst = re.sub(r"PYTHON\({(.*?)}\)", repl_python, src, flags=re.DOTALL)
    return dst

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Process some files.")
    parser.add_argument('source', help="Source file to process")
    parser.add_argument('-o', '--output', default=None, help="Output build directory")

    args = parser.parse_args()
    base_name = os.path.basename(args.source)
    build_dir = args.output
    if (build_dir == None):
        build_dir = base_name.rstrip(".c") + "_build"
    out_fn = base_name.rstrip(".c") + "_mario.c"    
    output_cstr = preprocess(args.source)
    # make build directory, copy resources, write file to there
    print(f"Compiling to build directory: {build_dir}")
    os.makedirs(build_dir, exist_ok=True)
    copy_dpdk_resources(build_dir)
    # note: the included makefile assumes the c file is named mario.c
    out_fn = os.path.join(build_dir, out_fn)
    with open(out_fn, 'w') as f:
        f.write(output_cstr)
