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

# mac table type and constructor
mactbl_t = UserTy(
    name = "mactbl_t",
    cstr = """
typedef struct { uint8_t dmac[6]; uint16_t port; } mactbl_entry_t;
typedef struct { int len; mactbl_entry_t *entries;} mactbl_t;
"""
)

# annoying: how do we know what parameters mactbl_state needs?
# (to call the init function. This should actually have a type
# that matches the init function, not the state itself)
mactbl_state = AtomState(
    ty = ref[mactbl_t],
    name = "mactbl_init",
    cstr = """
mactbl_t* mactbl_init(int len) {
    mactbl_t* tbl = calloc(1, sizeof(mactbl_t));
    tbl->entries = calloc(len, sizeof(mactbl_entry_t));
    tbl->len = len;
    return tbl;
}"""
)

mactbl_get = Atom[mactbl_state(1024), ref[macaddr_t], ref[uint16_t]](
    name="mactbl_get",
    cstr="""
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
""")

mactbl_set = Atom[mactbl_state(1024), ref[macaddr_t], ref[uint16_t]](
    name="mactbl_set",
    cstr="""
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
""")


# not bound to a specific location
raw_mac_pipe = Pipe(
    # get source mac, dst mac, and ingress port
    # lookup dst_port = tbl[dst_mac]
    # set tbl[src_mac] = ingress_port
    (Meta.dmac <- extract_dmac() %
    (Meta.smac <- extract_smac() %
    (Meta.ingress_port <- get_ingress_port() %
    (Meta.out_port <- mactbl_get[Bank.mactbl](Meta.dmac) %
    (mactbl_set[Bank.mactbl](Meta.smac, Meta.ingress_port) >>
    Send(Meta.out_port))))))
)


# single core version
singlecore_mac = Main(
    includes=[hashfun],
    pipes = {
        eth0: At(Core.c0, raw_mac_pipe)     
    }
)

build(singlecore_mac, "singlecore_mac_build")