#!/usr/bin/env python3

# preliminary examples
# Note: these atoms are not implemented

import sys, os, subprocess
sys.path.append("./lib")

from lib.compile import *
from lib.names import *
from lib.ty import *
from lib.usersyntax import *
from lib.stdlib import * 



# StarFlow -- a telemetry cache that exports TCP/IP flow records that contain vectors of 
# per packet features. Grouping the features by flow makes them easier to process at the 
# backend.
starflow = Pipe(
    Meta.hdr == Atom.parse() % # parse the packet header
    Meta.idx == Atom.hash(Meta.hdr) % # get the index into the flow table by hashing flow key
    Meta.flow_counters == Atom.update_flow_metrics(Meta.idx, Meta.hdr) % # update flow-level features, e.g., byte and packet counts
    Meta.pkt_features  == Atom.append_pkt_features(Meta.idx, Meta.hdr) % # append a compact packet record to a vector for this flow
    # check if its time to export the record for this flow, e.g., 
    # because it has been too long since last export, 
    # or because we are running out of memory
    Meta.export_needed == Atom.check_export(Meta.flow_counters) % 
    Switch(Meta.export_needed, {
        # if an export is needed: build a flow record; figure out the port where the collector lives; 
        # replace the packet's contents with the record; send the packet to the collector port.
        1: (Meta.flow_record == Atom.build_record(Meta.hdr, Meta.flow_counters, Meta.pkt_features) %
            Meta.collector_port == Atom.select_collector(Meta.hdr) %
            Atom.overwrite_packet(Meta.record) >> Atom.send(Meta.collector_port)),
        None:()
    })
)


# MAC learner. 
# the mac learner looks up the destination address of each packet 
# to find an output port. Then, it records the mapping from 
# address -> port for the packets source address and ingress port.
mac = Pipe(
    Meta.hdr == Atom.parse()                              %
    Meta.ingress_port == Atom.get_ingress_port()          %
    Meta.out_port == Atom.lookup_dst["mac_tbl"](Meta.hdr) %
    (Atom.update_src["mac_tbl"](Meta.hdr, Meta.ingress_port) >>
     Atom.send(Meta.out_port))
)

# NAT -- a network address translator. 
# The nat translates private internal ip addresses into public internet ip addresses. 
# When an internal -> external packet arrives, the nat checks to see if the internal address
# is bound to an external one from its pool. If there is an external address, rewrite the 
# source. Else, grab a new address, update the table, then rewrite the src.
# When an external -> internal packet arrives, the nat does a reverse lookup on the 
# destination address and rewrites the packets destination address with that value. 
nat = Pipe(
    Meta.hdr == Atom.parse() %
    Meta.direction == Atom.get_flow_direction(Meta.hdr) %
    Switch(Meta.direction, {
        0 : ( # from internal host behind NAT to public internet
            Meta.src_addr == Atom.get_src_addr(Meta.hdr) % 
            # check if this source address has already been allocated a public IP
            Meta.has_public_addr == Atom.exists["nat_table"](Meta.src_addr) % 
            Switch(Meta.has_public_addr), {
                0 : ( # no previously assigned public address: 
                      # get one from addr pool, update nat_table, rewrite source, 
                      # lookup output port, send
                    Meta.new_src_addr == Atom.get_fresh_src["addr_pool"]() %
                    (Atom.set["nat_table"](Meta.new_src_addr, Meta.hdr) >>
                    Atom.update_packet_src(Meta.new_src_addr)) >>
                    Meta.out_port == Atom.lookup(Meta.hdr) %
                    Atom.send(Meta.out_port)                    
                ),
                1 : (
                    # previously assigned public address: 
                    # lookup output port, send
                    Meta.new_src_addr == Atom.get["nat_table"](Meta.src_addr) %
                    (Atom.update_packet_src(Meta.new_src_addr)) >>
                    Meta.out_port == Atom.lookup(Meta.hdr) %
                    Atom.send(Meta.out_port)                    
                )
            }),        
        1 : ( # from public internet to internal host behind NAT
            # replace destination address, which is a public address from our pool, 
            # with the private address that the public address is currently bound to
            Meta.dst_addr == Atom.dst_addr(Meta.hdr) %
            Meta.orig_hdr == Atom.get_key_from_value["nat_table"](Meta.dst_addr) %
            Atom.update_packet_hdr(Meta.orig_hdr) >>
            Meta.out_port == Atom.lookup(Meta.orig_hdr) %
            Atom.send(Meta.out_port)            
            )
     })
)

