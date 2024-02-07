#!/usr/bin/env python3

# This script splits a pcap file into n files, round-robin style
# the output files are named <inputfile>.<n>.pcap

import sys
import os
import dpkt
import argparse

if __name__ == '__main__':

    parser = argparse.ArgumentParser(description='Split a pcap file into n files, round-robin style')
    parser.add_argument('inputfile', help='input pcap file')
    parser.add_argument('n', type=int, help='number of output files')
    args = parser.parse_args()

    inputfile = args.inputfile
    n = args.n

    if not os.path.isfile(inputfile):
        print('Input file not found')
        sys.exit(1)

    f = open(inputfile, 'rb')
    pcap = dpkt.pcap.Reader(f)

    outputfiles = [open('%s.%d.pcap' % (inputfile, i), 'wb') for i in range(n)]
    outputwriters = [dpkt.pcap.Writer(f) for f in outputfiles]

    i = 0
    for ts, buf in pcap:
        outputwriters[i].writepkt(buf, ts)
        i = (i + 1) % n

    for f in outputfiles:
        f.close()
    f.close()