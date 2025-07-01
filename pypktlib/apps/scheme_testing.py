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

# Example programs for schemes

from lib.compile import *
from lib.stdlib import * 
from lib.scheme import *
from lib.usersyntax import *

countifethodd = Atom[ref[counter_state_ty], ref[eth_t], ref[uint32_t]](
    state=make_counter(1),
    name="countifethodd",
    cstr="""
void count(counter_state_t* state, char* pkt, uint32_t* count) {
        // update counter and return
        if (eht->dst[5] & 1 == 1) {
            (*state->counter)++;
            *count = *state->counter;
        }
    }"""
)


#This pipe parses the ethernet header, then counts the packets that have odd eth dest field and prints a running count
p1 = (Meta.eth <- parse_eth() % 
        (Meta.ct <- countifethodd(Meta.eth) %
        (print_str("ip ") >> print_ct(Meta.ct) >> Exit(eth0))))

full_par_scheme = FullParallel(compute_locations=[Core.c1, Core.c2])
full_parallel_count = Main(
    pipes = {
        eth0: Transform(pipe=p1, scheme=full_par_scheme)
    }
)

par_shared_scheme = ParallelShared(compute_locations=[Core.c1, Core.c2], shard_location=Core.c0)
parallel_shared_count = Main(
    pipes= {
        eth0: (Transform(pipe=p1, scheme=par_shared_scheme))
    }
)
#----------------------------------------------------------------
full_par_scheme2 = FullParallel(compute_locations=[Core.c1, Core.c2])
scr_scheme = SCRDeannotate()
inner_p = Meta.ct <- countifethodd(Meta.eth)
post_p = (print_str("ip ") >> print_ct(Meta.ct) >> Exit(eth0))
p2 = Transform(inner_p, scr_scheme) % post_p
p3 = Transform(p2, full_par_scheme2)
p4 = (Meta.eth <- parse_eth() %
        Transform(p3, scr_scheme.annotator(Meta.eth, 2)))
scr_annotate_count = Main(
    pipes= {
        eth0: p4
    }
)
build(scr_annotate_count, "parallel_count")
