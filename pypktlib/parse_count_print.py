#!/usr/bin/env python3
import sys, os, subprocess
sys.path.append("./lib")

from lib.compile import *
from lib.names import *
from lib.ty import *
from lib.usersyntax import *
from lib.stdlib import * 


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