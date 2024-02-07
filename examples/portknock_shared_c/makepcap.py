#!/usr/bin/env python3

# this script generates a pcap file from a json file that describes a sequence of packets.

import sys, time, random, socket, os, struct, json, copy
import binascii
import dpkt
from collections import namedtuple

usage = "usage: makepcap.py testspec.json pkts.pcap"
def main():
    generatePcap(sys.argv[1], sys.argv[2])

defaultPktRec = {
    "eth.src":"12:12:12:12:12:12",
    "eth.dst":"34:34:34:34:34:34",
    "eth.type":dpkt.ethernet.ETH_TYPE_IP,
    "ip.src" : "12.12.12.12",
    "ip.dst" : "34.34.34.34",
    "ip.id"  : 0,
    "ip.tos" : 0,
    "ip.ttl" : 64,
    "ip.proto" : "tcp",
    "tcp.src" : 10,
    "tcp.dst" : 20,
    "payload.length" : 128
}
def generatePcap(testspecfn, pcapFn):
    # parse trace json file.
    traceJson = json.load(open(testspecfn))
    templateRecs = traceJson["packet_templates"]
    # load the template packet records
    for (name, templateRec) in templateRecs.items():
        # grab defaults for any fields that are not filled in. 
        rec = {field:templateRec.get(field, defaultPktRec[field]) for field in defaultPktRec.keys()}
        templateRecs[name].update(rec)
    # print("--- packet templates ---")
    # for (name, templateRec) in templateRecs.items():
    #     print ("[%s] %s" % (name, templateRec))
    specRecs = traceJson["packets"]
    packetRecs = []
    for specRec in specRecs:
        default_rec = copy.deepcopy(defaultPktRec)
        # use the template as base, if there is one
        if (specRec["template"]):
            default_rec.update(templateRecs[specRec["template"]])
        # update the defaults with the spec
        rec = {field:specRec.get(field, default_rec[field]) for field in default_rec.keys()}
        packetRecs.append(rec)
    pkts = [pktFromRec(p) for p in packetRecs]

    # write the packets to the output pcap file.
    pw = PcapWriter()
    pw.create(pcapFn)
    for (i, pkt) in enumerate(pkts):
        pw.write(pkt, i*4) # packets are 4 seconds apart
    pw.close()


def fmt_ipaddr(addr):
    if (isinstance(addr, str)):
        return socket.inet_aton(addr)
    elif (isinstance(addr, int)):
        return struct.pack('!L', addr)

def pktFromRec(rec):
    """ convert a pktRec into a packet """
    # craft a L4 packet (tcp / udp)
    L4Packet = None
    if (rec["eth.type"] == dpkt.ethernet.ETH_TYPE_IP):
        if (rec["ip.proto"] == "udp"):
            payload = b'X'*int(rec["payload.length"])
            udpPkt = dpkt.udp.UDP(
                sport = int(rec["udp.src"]), 
                dport = int(rec["udp.dst"]))
            udpPkt.data = payload
            udpPkt.ulen += len(udpPkt.data)
            L4Packet = bytes(udpPkt)
        elif (rec["ip.proto"] == "tcp"):
            payload = b'X'*int(rec["payload.length"])
            tcpPkt = dpkt.tcp.TCP(
                sport = int(rec["tcp.src"]), 
                dport = int(rec["tcp.dst"]),
                data = payload
            )
            tcpPkt.sum = 0  # let dpkt compute the checksum
            L4Packet = bytes(tcpPkt)

        else:
            print ("ERROR: NON TCP/UDP PACKETS NOT IMPLEMENTED")
            quit()

    L3Packet = None

    # craft a L3 packet (ip)
    if (rec["eth.type"] == dpkt.ethernet.ETH_TYPE_IP):
        src = None
        proto = rec["ip.proto"]
        if (proto == "tcp"):
            proto = dpkt.ip.IP_PROTO_TCP
        elif (proto == "udp"):
            proto = dpkt.ip.IP_PROTO_UDP

        ipPkt = dpkt.ip.IP(v=4, 
            id = int(rec["ip.id"]), 
            src = fmt_ipaddr(rec["ip.src"]), 
            dst = fmt_ipaddr(rec["ip.dst"]), 
            p   = int(proto),
            tos = int(rec["ip.tos"]), 
            ttl = int(rec["ip.ttl"])
        )
        ipPkt.data = L4Packet
        ipPkt.len += len(L4Packet)
        L3Packet = bytes(ipPkt)
    else:
        print ("ERROR: NON IP PACKETS NOT IMPLEMENTED")
        quit()

    # craft a L2 frame (ethernet)
    hexStrToBin = lambda ethAddr : binascii.unhexlify(ethAddr.replace(":", ""))
    L2Frame = None
    ethFrame = dpkt.ethernet.Ethernet(
        src=hexStrToBin(rec["eth.src"]), 
        dst=hexStrToBin(rec["eth.dst"]), 
        type=int(rec["eth.type"])
    )
    ethFrame.data = L3Packet
    L2Frame = bytes(ethFrame)    
    return L2Frame


class PcapWriter(object):
    """ Write bytestring packets into a PCAP file."""
    # global header for a version 2.4 PCAP file.
    pcap_global_header=bytes.fromhex('d4c3b2a1') + struct.pack("H",2) + struct.pack("H",4) + struct.pack("I", 0) + struct.pack("I", 0) + struct.pack("I", 1600) + struct.pack("I", 1)
    def __init__(self, f = None):
        if (f):
            self.f = f
            self.f.write(self.pcap_global_header)            
        return

    def create(self, filename):
        self.f = open(filename, "wb")
        self.f.write(self.pcap_global_header)

    def write(self, pkt, tsSec):
        """
        Writes an ethernet packet to a file. Prepends the pcap packet header.
        """
        pcap_len = len(pkt)
        seconds = int(tsSec)
        microseconds = int((tsSec - int(tsSec)) * 1000000)
        pktBytes = struct.pack("<i",seconds) + struct.pack("<i",microseconds) + struct.pack("<i", pcap_len) + struct.pack("<i", pcap_len) + bytes(pkt)
        self.f.write(pktBytes)

    def close(self):
        self.f.close()




if __name__ == '__main__':
    main()