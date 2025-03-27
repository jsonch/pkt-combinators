#!/usr/bin/env python3
# simple test for projections (struct fields) as 
# atom arguments
# supporting nested structs, too!
import sys, os, subprocess

def append_git_root_subdir(sub_path):
    try:
        root = subprocess.check_output(["git", "rev-parse", "--show-toplevel"]).strip().decode('utf-8')
        sys.path.append(f"{root}/{sub_path}")
    except subprocess.CalledProcessError:
        print("This directory is not part of a git repository.")
        sys.exit(1)

append_git_root_subdir("pypktlib")

from lib.compile import *
from lib.usersyntax import *
from lib.stdlib import * 


nested_t = UserTy("nested_t",
"""
typedef struct inner_t {
    int x;
    int y;
} inner_t;
typedef struct nested_t {
    inner_t inner;
} nested_t;
"""
)

set_inner = Atom[None, None, ref[nested_t]]("set_inner",
"""
void set_inner(void * nostate, char * pkt, nested_t* n) {
    n->inner.x = 1;
    n->inner.y = 2;
}
""")

print_int = Atom[None, ref[int_t], None]("print_int",
"""
void print_int(void * nostate, char * pkt, int* n) {
    printf("n: %d\\n", *n);
}
""")
nested_struct = Main(
    pipes = {
        eth0:Move(Core.c0) >>
            Meta.nested <- set_inner()             %
            print_int(Meta.nested.inner.x)
            }
)


build(nested_struct, "nested_struct")