#!/bin/bash
if [ "$#" -ne 2 ]; then
    echo "Usage: $0 <input_pcap> <output_pcap>"
    exit 1
fi
input_pcap=$1
output_pcap=$2
sudo ./build/*-shared --log-level=8 -l 0-2 -n 4 --no-pci --vdev 'net_pcap0,rx_pcap='$input_pcap',tx_pcap='$output_pcap