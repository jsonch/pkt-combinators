#!/usr/bin/env python3
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


# read from eth0, parse eth and count ip packets, print ip count, forward to eth1
parse_ct_print = Main(
    pipes = {
        eth0:At(Core.c0,
            Meta.eth <- parse_eth()              %
            (Meta.ety <- get_eth_ty(Meta.eth)    %
            Switch(Meta.ety)({
                0x0800: (
                    Meta.ct <- counter[Bank.foo]() %
                    (print_str("ip ") >> print_ct(Meta.ct) >> Exit(eth0))), 
                None:(
                    Meta.ct <- counter[Bank.bar]() %
                    (print_str("not ip ") >> print_ct(Meta.ct) >> Exit(eth0)))                 
            }))
        )
    }
)
# build(parse_ct_print, "parse_ct_print")

# try it with a move instead of an At
parse_ct_print_move = Main(
    pipes = {
        eth0:
            (
                Move(Core.c0) >>
                Meta.eth <- parse_eth()              %
                (Meta.ety <- get_eth_ty(Meta.eth)    %
                Switch(Meta.ety)({
                    0x0800: (
                        Meta.ct <- counter[Bank.foo]() %
                        (print_str("ip ") >> print_ct(Meta.ct) >> Exit(eth0))), 
                    None:(
                        Meta.ct <- counter[Bank.bar]() %
                        (print_str("not ip ") >> print_ct(Meta.ct) >> Exit(eth0)))
                }))
            )
    }
)

def print_ip():
    ip_pipe = Pipe(
        Meta.ct <- counter[Bank.ip]() %
        (print_str("ip ") >> print_ct(Meta.ct) >> Exit(Eth(0)))
    )
    return ip_pipe

def print_non_ip():
    non_ip_pipe = Pipe(
        Meta.ct <- counter[Bank.nonip]() %
        (print_str("non-ip ") >> print_ct(Meta.ct) >> Exit(Eth(0)))
    )
    return non_ip_pipe
mod_state_ty = UserTy(
    name = "mod_state_t",
    cstr = """
typedef struct mod_state_t {
    uint32_t* counter;
} mod_state_t;
""")

make_mod = StateInit[uint32_t, ref[mod_state_ty]](
    name = "mod_init",
    cstr = """
mod_state_t* mod_init(int len){
    mod_state_t* state = malloc(sizeof(mod_state_t));
    state->counter = mmap(NULL, len * sizeof(uint32_t *), PROT_READ | PROT_WRITE, MAP_SHARED | MAP_ANONYMOUS, -1, 0);
    for (int i = 0; i < len; i++){
        state->counter[i] = 0;
    }
    return state;
}""")
def round_robin_counter(n):
    return Atom[ref[mod_state_ty], None, ref[intpair_t]](
    state=make_mod(1),
    name="counter_and_mod",
    cstr=f"""
    void counter_and_mod(mod_state_t* state, char* pkt, intpair_t* result) {{
            // update mod and return
            (*state->counter)++;
            result->count = *state->counter;
            result->mod = *state->counter % {n};
        }}
        """
    )
intpair_t = UserTy(
    name = "intpair_t",
    cstr = 
"""
typedef struct intpair_t {
    uint32_t count;
    uint32_t mod;
} intpair_t;
"""
)
# # read from eth0, parse and switch if ip/not. shard to three cores to print. exit to eth0.
parse_ct_shard_print = Main(
    pipes = {
        eth0:
            (
                Move(Core.c0) >>
                Meta.eth <- parse_eth()              %
                (Meta.ety <- get_eth_ty(Meta.eth)    %
                Switch(Meta.ety)({
                    0x0008: (
                        Meta.result <- round_robin_counter(3)[Bank.foo]()      %
                        Switch(Meta.result.mod)({
                            0: Move(Core.c1) >> print_ip(),
                            1: Move(Core.c2) >> print_ip(),
                            2: Move(Core.c3) >> print_ip()
                        })
                    ),
                    None: (
                        Move(Core.c4) >> print_non_ip()
                    )
                }))
            )
    }
)

build(parse_ct_shard_print, "scheme-3")