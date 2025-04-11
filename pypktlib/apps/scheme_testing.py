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


p1 = At(Core.c0, 
        Meta.eth <- parse_eth() %
        (Meta.ct <- counter() %
        (print_str("ip ") >> print_ct(Meta.ct) >> Exit(eth0))))

full_par_scheme = FullParallel(compute_locations=[Core.c1, Core.c2])
full_parallel_count = Main(
    pipes = {
        eth0: Transform(pipe=p1, scheme=full_par_scheme)
    }
)

par_shared_scheme = ParallelShared(compute_locations=[Core.c1, Core.c2])
parallel_shared_count = Main(
    pipes= {
        eth0: Transform(pipe=p1, scheme=par_shared_scheme)
    }
)
build(parallel_shared_count, "parallel_shared_count")