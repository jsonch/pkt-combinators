#!/usr/bin/env python3

import sys
import os
import usersyntax
import syntax

# from mario import *

# # a bunch of test programs
# def test():
#     parse_atom = atom(None, None, "parsed_pkt_t", "parse", [])
#     counter_atom = atom("counter_state_t", "counter_init", "counter_ret_t", "counter", [])
#     fwd_atom = atom("fwd_state_t", "fwd_init", "fwd_ret_t", "fwd", ["parsed_pkt_t", "counter_ret_t"])
#     p1 =at("c0", 
#             let("parsed_pkt", do(parse_atom, []),
#             at("c1", 
#                 let("ct", do(counter_atom, ["parsed_pkt"]),
#                 seq(
#                 do(fwd_atom, ["parsed_pkt", "ct"]),
#                 fwd("eth0")
#     )))));
#     print("--- initial program--- ")
#     print(pipe_to_string(p1))
#     ret_str = compile("p1", p1)
#     print("Generated code:")
#     print(ret_str)


# def simple_move():
#     foo_atom = atom(None, None, "foo_ret_t", "foo", [], "//todo", "//todo")
#     bar_atom = atom(None, None, "bar_ret_t", "bar", [], "//todo", "//todo")
#     pipe=at("c0",
#         seq(
#             at ("c1", do(foo_atom, [])),
#             at ("c2", do(bar_atom, [])),
#         )
#     )
#     print("--- initial program--- ")
#     print(pretty_print(pipe))
#     ret_str = compile("pipe", pipe)
#     print("Generated code:")
#     print(ret_str)

# def simple_move2():
#     foo_atom = atom(None, None, "foo_ret_t", "foo", [])
#     bar_atom = atom(None, None, "bar_ret_t", "bar", [])
#     pipe=at("c0",
#         seq(
#             at ("c1", do(foo_atom, [])),
#             do(bar_atom, []),
#         )
#     )
#     print("--- initial program--- ")
#     print(pretty_print(pipe))
#     ret_str = compile("pipe", pipe)
#     # print("Generated code:")
#     # print(ret_str)
# def self_at():
#     foo_atom = atom(None, None, "foo_ret_t", "foo", [])
#     bar_atom = atom(None, None, "bar_ret_t", "bar", [])
#     pipe=at("c0",
#         seq(
#             at ("c1", do(foo_atom, [])),
#             at ("c1", do(bar_atom, [])),
#         )
#     )
#     print("--- initial program--- ")
#     print(pretty_print(pipe))
#     ret_str = compile("pipe", pipe)

# def let_vars():
#     foo_atom = atom(None, None, "foo_ret_t", "foo", [])
#     bar_atom = atom(None, "bar_ret_t", "bar", ["foo_ret_t"])
#     pipe=at("c0",
#         let("foo_result", do(foo_atom, []),
#         do(bar_atom, ["foo_result"])
#         )
#     )
#     print("--- initial program--- ")
#     print(pretty_print(pipe))
#     ret_str = compile("pipe", pipe)

# def let_move_use():
#     foo_atom = atom(None, None, "foo_ret_t", "foo", [])
#     bar_atom = atom("bar_state_t", "bar_init", "bar_ret_t", "bar", ["foo_ret_t"])
#     pipe=at("c0",
#         let("foo_result", do(foo_atom, []),
#         at("c1", do(bar_atom, ["foo_result"]))
#         )
#     )
#     print("--- initial program--- ")
#     print(pretty_print(pipe))
#     ret_str = compile("pipe", pipe)
#     print("Generated code:")
#     print(str(ret_str))

# def from_nic():
#     foo_atom = atom(None, None, "foo_ret_t", "foo", [])
#     bar_atom = atom("bar_state_t", "bar_init","bar_ret_t", "bar", ["foo_ret_t"])
#     c0_pipe=at("c0",
#         let("foo_result", do(foo_atom, []),
#         at("c1", do(bar_atom, ["foo_result"]))
#         )
#     )    
#     full_pipe = nic([(("eth0", 0), c0_pipe)])
#     print("--- initial program--- ")
#     print(pretty_print(full_pipe))
#     ir_prog = compile("pipe", full_pipe)
#     print("Generated code:")
#     print(str(ir_prog))

def test_dpdk():
    prog_name = "prog1"
    directory = "/Users/jsonch/Desktop/gits/dpdk-hacking/mario"
    foo_atom = atom(None, None, "foo_ret_t", "foo", [], "//todo", "//todo", [])
    bar_atom = atom("bar_state_t", "bar_init","bar_ret_t", "bar", ["foo_ret_t"], "//todo", "//todo", [])
    c0_pipe=at("c0",
        let("foo_result", do(foo_atom, []),
        at("c1", do(bar_atom, ["foo_result"]))
        )
    )    
    full_pipe = nic([(("eth0", 0), c0_pipe)])
    prog = compile_dpdk(full_pipe)
    print("---------")
    print(prog)
    print("---------")

from names import *

import usersyntax as us

def atom_translate():
    foo_atom = us.Atom[None, None, ref[Ty.foo_ret_t]](
        name="foo",
        f="""//todo: foo impl"""
    )
    # foo_atom = atom(None, None, "foo_ret_t", "foo", [], "//todo", "//todo", [])


import frontend
import backend
def compile_dpdk(pipe:syntax.PipeBase):
    pipe_prog = frontend.frontend_passes(pipe)
    segment_prog = backend.backend_passes(pipe_prog)
    return backend.irprog_to_dpdkcode(segment_prog)


def newfrontend_test():

    foo_atom = us.Atom[None, None, ref[Ty.foo_ret_t]](
        name="foo",
        f="""//todo: foo impl"""
    )
    print("----foo----")
    ir_foo_atom = foo_atom.to_ir()
    print("-----")
    print(ir_foo_atom)
    print("----callfoo----")
    callfoo = us.Pipe(foo_atom())
    print(callfoo)
    print("-----")
    print(callfoo.to_ir())

    bar_atom = us.Atom[ref[Ty.bar_state_t], (ref[Ty.foo_ret_t]), ref[Ty.bar_ret_t]](
        name = "bar", 
        f = "//todo: bar impl",
        initname = "bar_init",
        init = "//todo: bar init impl",
        init_args = []
    )
    print("----bar-----")
    print(bar_atom.to_ir())

    c0_pipe = us.Pipe(
        us.Meta.foo_result <- foo_atom() %
        bar_atom(us.Meta.foo_result)
    )

    print("----c0_pipe----")
    c0_ir = c0_pipe.to_ir()

    # print(c0_ir)
    print(syntax.pretty_print(c0_ir))
    # foo_atom = atom(None, None, "foo_ret_t", "foo", [], "//todo", "//todo", [])
    # bar_atom = atom("bar_state_t", "bar_init","bar_ret_t", "bar", ["foo_ret_t"], "//todo", "//todo", [])
    # c0_pipe=at("c0",
    #     let("foo_result", do(foo_atom, []),
    #     at("c1", do(bar_atom, ["foo_result"]))
    #     )
    # )    
    full_pipe = syntax.nic([(("eth0", 0), c0_ir)])
    prog = compile_dpdk(full_pipe)
    print("---------")
    print(prog)
    print("---------")



if __name__ == '__main__':
    # test()
    # test_switch()
    # simple_move2()
    # self_at()
    # let_vars()
    # let_move_use()
    # from_nic()
    # test_dpdk()
    newfrontend_test()

