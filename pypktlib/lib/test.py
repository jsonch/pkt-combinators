#!/usr/bin/env python3
from mario import *

# a bunch of test programs
def test():
    parse_atom = atom(None, None, "parsed_pkt_t", "parse", [])
    counter_atom = atom("counter_state_t", "counter_init", "counter_ret_t", "counter", [])
    fwd_atom = atom("fwd_state_t", "fwd_init", "fwd_ret_t", "fwd", ["parsed_pkt_t", "counter_ret_t"])
    p1 =at("c0", 
            let("parsed_pkt", do(parse_atom, []),
            at("c1", 
                let("ct", do(counter_atom, ["parsed_pkt"]),
                seq(
                do(fwd_atom, ["parsed_pkt", "ct"]),
                fwd("eth0")
    )))));
    print("--- initial program--- ")
    print(pipe_to_string(p1))
    ret_str = compile("p1", p1)
    print("Generated code:")
    print(ret_str)


def test_switch():
    # pipeline with a switch statement in it
    parse_atom = atom(None, None, "parsed_pkt_t", "parse", [])
    counter_atom = atom("counter_state_t", "counter_init", "counter_ret_t", "counter", [])
    fwd_atom = atom("fwd_state_t", "fwd_init", "fwd_ret_t", "fwd", ["parsed_pkt_t", "counter_ret_t"])
    p1=at("c0",
        let("parsed_pkt", do(parse_atom, []),
        let("ct", do(counter_atom, ["parsed_pkt"]),
        seq(
        switch("ct",
            {0:at("c1", seq(do(fwd_atom, ["parsed_pkt", "ct"]), drop())),
              1: at("c2", seq(do(fwd_atom, ["parsed_pkt", "ct"]), drop()))}),
        fwd("eth2")
        ))))
    print("--- initial program--- ")
    print(pretty_print(p1))
    ret_str = compile("p1", p1)
    print("Generated code:")
    print(ret_str)

def simple_move():
    foo_atom = atom(None, None, "foo_ret_t", "foo", [])
    bar_atom = atom(None, None, "bar_ret_t", "bar", [])
    pipe=at("c0",
        seq(
            at ("c1", do(foo_atom, [])),
            at ("c2", do(bar_atom, [])),
        )
    )
    print("--- initial program--- ")
    print(pretty_print(pipe))
    ret_str = compile("pipe", pipe)
    print("Generated code:")
    print(ret_str)

def simple_move2():
    foo_atom = atom(None, None, "foo_ret_t", "foo", [])
    bar_atom = atom(None, None, "bar_ret_t", "bar", [])
    pipe=at("c0",
        seq(
            at ("c1", do(foo_atom, [])),
            do(bar_atom, []),
        )
    )
    print("--- initial program--- ")
    print(pretty_print(pipe))
    ret_str = compile("pipe", pipe)
    # print("Generated code:")
    # print(ret_str)
def self_at():
    foo_atom = atom(None, None, "foo_ret_t", "foo", [])
    bar_atom = atom(None, None, "bar_ret_t", "bar", [])
    pipe=at("c0",
        seq(
            at ("c1", do(foo_atom, [])),
            at ("c1", do(bar_atom, [])),
        )
    )
    print("--- initial program--- ")
    print(pretty_print(pipe))
    ret_str = compile("pipe", pipe)

def let_vars():
    foo_atom = atom(None, None, "foo_ret_t", "foo", [])
    bar_atom = atom(None, "bar_ret_t", "bar", ["foo_ret_t"])
    pipe=at("c0",
        let("foo_result", do(foo_atom, []),
        do(bar_atom, ["foo_result"])
        )
    )
    print("--- initial program--- ")
    print(pretty_print(pipe))
    ret_str = compile("pipe", pipe)

def let_move_use():
    foo_atom = atom(None, None, "foo_ret_t", "foo", [])
    bar_atom = atom("bar_state_t", "bar_init", "bar_ret_t", "bar", ["foo_ret_t"])
    pipe=at("c0",
        let("foo_result", do(foo_atom, []),
        at("c1", do(bar_atom, ["foo_result"]))
        )
    )
    print("--- initial program--- ")
    print(pretty_print(pipe))
    ret_str = compile("pipe", pipe)
    print("Generated code:")
    print(str(ret_str))


def from_nic():
    foo_atom = atom(None, None, "foo_ret_t", "foo", [])
    bar_atom = atom("bar_state_t", "bar_init","bar_ret_t", "bar", ["foo_ret_t"])
    c0_pipe=at("c0",
        let("foo_result", do(foo_atom, []),
        at("c1", do(bar_atom, ["foo_result"]))
        )
    )    
    full_pipe = nic([(("eth0", 0), c0_pipe)])
    print("--- initial program--- ")
    print(pretty_print(full_pipe))
    ir_prog = compile("pipe", full_pipe)
    print("Generated code:")
    print(str(ir_prog))

def test_cprint():
    foo_atom = atom(None, None, "foo_ret_t", "foo", [])
    bar_atom = atom("bar_state_t", "bar_init","bar_ret_t", "bar", ["foo_ret_t"])
    c0_pipe=at("c0",
        let("foo_result", do(foo_atom, []),
        at("c1", do(bar_atom, ["foo_result"]))
        )
    )    
    # full_pipe = nic([(("eth0", 0), c0_pipe)])
    ir_prog = compile("pipe", c0_pipe)
    print(ir_prog)
    queue_to_structdef(ir_prog.queues[0])
    segment1 = ir_prog.segments[0]
    print("Segment 1 pipe")
    print(segment1.pipe)
    print (queue_to_structdef(ir_prog.queues[0]))


def compile_dpdk(prog_name: str, tgt_dir: str, pipe:PipeBase):
    ir_prog = compile(prog_name, pipe)
    dpdk_emit(prog_name, tgt_dir, ir_prog)

def test_dpdk():
    prog_name = "prog1"
    directory = "/Users/jsonch/Desktop/gits/dpdk-hacking/mario"
    foo_atom = atom(None, None, "foo_ret_t", "foo", [])
    bar_atom = atom("bar_state_t", "bar_init","bar_ret_t", "bar", ["foo_ret_t"])
    c0_pipe=at("c0",
        let("foo_result", do(foo_atom, []),
        at("c1", do(bar_atom, ["foo_result"]))
        )
    )    
    full_pipe = nic([(("eth0", 0), c0_pipe)])
    compile_dpdk(prog_name, directory, full_pipe)


if __name__ == '__main__':
    # test()
    # test_switch()
    # simple_move2()
    # self_at()
    # let_vars()
    # let_move_use()
    # from_nic()
    test_dpdk()

