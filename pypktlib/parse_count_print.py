#!/usr/bin/env python3

#!/usr/bin/env python3
import sys
import os

# Add the lib directory to the sys.path
sys.path.append(os.path.join(os.path.dirname(__file__), 'lib'))

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
            (Meta.ct <- count_ip(Meta.ety)       %
            Switch(Meta.ety)({
                0x0800: print_ct(Meta.ct) >> Exit(eth1),
                None: Exit(eth1)
            })))
        )
    }
)


build(parse_ct_print, "parse_ct_print")