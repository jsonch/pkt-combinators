#!/usr/bin/env python3

from compile import *
from names import *
from ty import *
from usersyntax import *

# type definitions
foo_ret_t = UserTy(
name="foo_ret_t",
cstr="""
typedef struct foo_ret_t {
    int x;
} foo_ret_t;
""")


# some atoms
foo_atom = Atom[None, None, ref[foo_ret_t]](
    name="foo",
    f="""//todo: foo impl"""
)()
bar_atom = Atom[ref[Ty.bar_state_t], (ref[foo_ret_t]), ref[Ty.bar_ret_t]](
    name = "bar", 
    f = "//todo: bar impl",
    initname = "bar_init",
    init = "//todo: bar init impl",
)()
bar2_atom = Atom[ref[Ty.bar_state_t], (ref[foo_ret_t]), ref[Ty.bar_ret_t]](
    name = "bar2",
    f = "//todo: bar2 impl",
    initname = "bar2_init",
    init = "void * bar2_init(int x, int y, int z) { return malloc(x + y + z); }"
)(1, 2, 3)

eth0 = (0, 0)
eth1 = (1, 0)
sending_pipe = Pipe(
    ((Meta.foo_result <- foo_atom() %
    Switch(Meta.foo_result)({
        0:bar_atom(1) >> Exit(eth0),                
        None:bar2_atom(Meta.foo_result) >> Exit(eth1) # none is the default case
    })) @ Core["c0"])            
)
main_pipe = Main(
    includes=[C("// some custom c helpers / imports / whatever")],
    pipes={eth0:sending_pipe})

def compile_sending_pipe():
    prog_str = compile_dpdk(main_pipe)
    print("----sending_pipe_prog-----")
    print(prog_str)
    print("----sending_pipe_prog-----")

def build_sending_pipe():
    build(main_pipe, "build_sending_pipe")


if __name__ == '__main__':
    # compile_sending_pipe()
    build_sending_pipe()
