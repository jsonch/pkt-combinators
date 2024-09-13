#!/usr/bin/env python3

""" 
A simple mac learner 
# the mac learner looks up the destination address of each packet 
# to find an output port. Then, it records the mapping from 
# address -> port for the packets source address and ingress port.
"""

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

# c helpers
hashfun = C("""
uint16_t calc_hash(uint8_t* key, uint32_t len, uint32_t maxval) {
    uint16_t hash = 0;
    for (uint32_t i = 0; i < len; i++) {
        hash = (hash << 5) + key[i];
    }
    return hash % maxval;
}
""")

# mac table type and constructor. 
# the constructor should really just be part of the type def...
mactbl_t = UserTy(
    name = "mactbl_t",
    cstr = """
typedef struct { uint8_t dmac[6]; uint16_t port; } mactbl_entry_t;
typedef struct { int len; mactbl_entry_t *entries;} mactbl_t;
"""
)

mactbl_init = C("""
mactbl_t* mactbl_init(int len) {
    mactbl_t* tbl = calloc(1, sizeof(mactbl_t));
    tbl->entries = calloc(len, sizeof(mactbl_entry_t));
    tbl->len = len;
    return tbl;
}""")

mactbl_get = Atom[ref[mactbl_t], ref[macaddr_t], ref[uint16_t]](
    name="mactbl_get",
    f="""
void mactbl_get(mactbl_t* tbl, char * pkt, uint8_t (*dmac)[6], uint16_t* port) {
    uint32_t idx = calc_hash(*dmac, 6, tbl->len);
    for (int i = 0; i < tbl->len; i++) {
        if (memcmp(tbl->entries[idx+i].dmac, *dmac, 6) == 0) {
            *port = tbl->entries[idx+i].port;
            return;
        }
    }
    *port = 0xFFFF; // Not found
}
""",
    initname="mactbl_init",
    init=str(mactbl_init)
)(1024)


mactbl_set = Atom[ref[mactbl_t], [ref[macaddr_t], ref[uint16_t]], None](
    name="mactbl_set",
    f="""
void mactbl_set(mactbl_t* tbl, char * pkt, uint8_t (*dmac)[6], uint16_t* port) {
    uint32_t idx = calc_hash(*dmac, 6, tbl->len);
    for (int i = 0; i < tbl->len; i++) {
        if (tbl->entries[idx+i].port == 0xFFFF) {
            memcpy(tbl->entries[idx+i].dmac, *dmac, 6);
            tbl->entries[idx+i].port = *port;
            return;
        }
    }
}
""",
    initname="mactbl_init",
    init=str(mactbl_init)
)(1024)


# not bound to a specific location
raw_mac_pipe = Pipe(
    # get source mac, dst mac, and ingress port
    # lookup dst_port = tbl[dst_mac]
    # set tbl[src_mac] = ingress_port
    (Meta.dmac <- extract_dmac() %
    (Meta.smac <- extract_smac() %
    (Meta.ingress_port <- get_ingress_port() %
    (Meta.out_port <- mactbl_get[State('mactbl')](Meta.dmac) %
    (mactbl_set[State('mactbl')](Meta.smac, Meta.ingress_port) >>
    Send(Meta.out_port))))))
)


# single core version
singlecore_mac = Main(
    includes=[hashfun],
    pipes = {
        eth0: At(Core["c0"], raw_mac_pipe)     
    }
)

build(singlecore_mac, "singlecore_mac_build")