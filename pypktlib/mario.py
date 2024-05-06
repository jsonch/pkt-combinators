#! /usr/bin/env python3
# minimal python prototype of combinator compiler
from dataclasses import dataclass
import re
import os, sys
import io
import contextlib

from syntax import *
from dpdk_compiler import *



### run python in PYTHON blocks of a c file
def preprocess(src_fn: str):
    # preprocess src_py.c to src.c, 
    # replacing all the PYTHON({...}) blocks with the 
    # print outputs of the python code
    base_name = os.path.basename(src_fn)
    dst_fn = os.path.splitext(base_name)[0].rstrip("_py") + ".c"
    print(f"Preprocessing {src_fn} to {dst_fn}")
    # Rest of the preprocessing logic goes here
    with open(src_fn, 'r') as f:
        src = f.read()
    def repl_python(match):
        code = match.group(1)
        # print(f"Running python code: {code}")
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
    with open(dst_fn, 'w') as f:
        f.write(dst)

if __name__ == '__main__':
    # call preprocess on the first arg
    if len(sys.argv) != 2:
        print("Usage: mario.py <source>")
        exit(1)
    src_fn = sys.argv[1]
    preprocess(src_fn)



### Test case
def test():
    # test case: 
    # a pipe that parses a packet, counts the number of packets, and forwards the packet
    parse_atom = atom(None, "parsed_pkt_t", "parse")
    counter_atom = atom("counter_state_t", "counter_ret_t", "counter")
    fwd_atom = atom("fwd_state_t", "fwd_ret_t", "fwd")

    p1 =let("parsed_pkt", pipe(parse_atom, []),
        let("ct", pipe(counter_atom, ["parsed_pkt"]),
        let("ct", pipe(counter_atom, ["parsed_pkt"]),    
        pipe(fwd_atom, ["parsed_pkt", "ct"]
    ))))
    # let "parsed_pkt" = pipe(parse_atom)() in
    # counter1 = counter_atom("parsed_pkt")
    # p1 = let "parsed_pkt" = parse()


    prog_string = compile("p1", p1)  
    print(prog_string) 
