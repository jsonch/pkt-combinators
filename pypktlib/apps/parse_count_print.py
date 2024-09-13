#!/usr/bin/env python3
import sys, os, subprocess

def git_root_subdir(sub_path):
    try:
        root = subprocess.check_output(["git", "rev-parse", "--show-toplevel"]).strip().decode('utf-8')
        sys.path.append(f"{root}/{sub_path}")
    except subprocess.CalledProcessError:
        print("This directory is not part of a git repository.")
        sys.exit(1)

sys.path.append(git_root_subdir("pypktlib/lib"))
from compile import *
from names import *
from ty import *
from usersyntax import *
from stdlib import * 


# read from eth0, parse eth and count ip packets, print ip count, forward to eth1
parse_ct_print = Main(
    pipes = {
        eth0:At(Core["c0"],
            Meta.eth <- parse_eth()             %
            (Meta.ety <- get_eth_ty(Meta.eth)    %
            Switch(Meta.ety)({
                0x0800: (
                    Meta.ct <- count() %
                    (print("ip ") >> print_ct(Meta.ct) >> Exit(eth0))), 
                None:(
                    Meta.ct <- count() %
                    (print("not ip ") >> print_ct(Meta.ct) >> Exit(eth0)))                 
            }))
        )
    }
)


build(parse_ct_print, "parse_ct_print")