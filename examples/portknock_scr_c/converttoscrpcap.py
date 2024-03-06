import socket
import ipaddress
from scapy.all import *
from scapy.utils import wrpcap
import argparse
import os

PKTS_WRITE_MAX_NUM = 100000
# typedef struct pkt_info_t {
#     uint32_t src_ip;
#     uint32_t dst_ip;
#     uint16_t src_port;
#     uint16_t dst_port;
# } pkt_info_t;
class MetadataElem():
  def __init__(self):
    self.src_ip = 0
    self.dst_ip = 0
    self.src_port = 0
    self.dst_port = 0

  def __str__(self):
    str += f"Source IP: {ipaddress.IPv4Address(self.src_ip)}\n"
    str += f"Dest IP: {ipaddress.IPv4Address(self.dst_ip)}\n"
    str += f"Source port: {self.src_port}\n"
    str += f"Dest port: {self.dst_port}\n"
    return str

  def __bytes__(self):
    # might need to modify ip from big-endian to little-endian
    md_bytes = self.src_ip.to_bytes(4, 'big')
    md_bytes += self.dst_ip.to_bytes(4, 'big')
    md_bytes += self.src_port.to_bytes(2, 'little')
    md_bytes += self.dst_port.to_bytes(2, 'little')
    return md_bytes

def get_md_from_pkt(pkt):
    md_elem = MetadataElem()
    md_elem.src_ip = int(ipaddress.ip_address(pkt.getlayer(IP).src))
    md_elem.dst_ip = int(ipaddress.ip_address(pkt.getlayer(IP).dst))
    if pkt.haslayer(TCP):
        md_elem.src_port = pkt.getlayer(TCP).sport
        md_elem.dst_port = pkt.getlayer(TCP).dport
    else:
        print(f"Unsupported layer type: {pkt.getlayer(IP).proto}")
        sys.exit(1)
    # print(md_elem)
    return md_elem

# Generator function to read and yield packets one by one
def read_packets(pcap_file):
    with PcapReader(pcap_file) as pcap_reader:
        for packet_number, packet in enumerate(pcap_reader, start=0):
            yield packet_number, packet

def gen_pcap_with_md_portknock(num_cores, dst_mac, output_path, input_file):
    print(f"start [gen_pcap_with_md_portknock] num_cores: {num_cores}")
    if not os.path.exists(output_path):
        os.makedirs(output_path)
    output_file = f"{output_path}/scr_{num_cores}.pcap"
    append_flag = False
    # input_pkts = rdpcap(input_file)
    new_pkts = list()
    md_initial = MetadataElem()
    pkt_history = []
    if num_cores > 1:
        pkt_history = [md_initial] * (num_cores - 1)
    for i, curr_pkt in read_packets(input_file):
        # print(f"\npkt {i}....")
        # get metadata from curr_pkt
        md_bytes = b''
        for x in pkt_history:
            md_bytes += bytes(x)
        # src_mac is used for rss and satisfying ethernet device
        src_mac = f"10:10:10:10:10:{format(i % num_cores, '02x')}"
        # print(src_mac)
        new_pkt = Ether(dst = dst_mac, src = src_mac, type=ETH_P_IP) / \
                  md_bytes / \
                  curr_pkt
        new_pkts.append(new_pkt)
        if num_cores > 1:
            curr_md = get_md_from_pkt(curr_pkt)
            # update pkt_history
            pkt_history = pkt_history[1:]
            pkt_history.append(curr_md)
        if len(new_pkts) >= PKTS_WRITE_MAX_NUM:
            wrpcap(output_file, new_pkts, append=append_flag)
            # print(f"Written {len(new_pkts)} packets to {output_pcap}")
            new_pkts = []
            append_flag = True
    if new_pkts:
        wrpcap(output_file, new_pkts, append=append_flag)
    print(f"[gen_pcap_with_md_portknock] output pcap: {output_file}")


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Information about parameters')
    parser.add_argument('--input', '-i', dest='input_file', help='Input file name', required=True)
    parser.add_argument("--output", "-o", dest="output_path", help="Output file name", required=True)
    parser.add_argument("--num_cores", "-n", dest="num_cores", help="Number of cores used to process packets", type=int, default=1)
    parser.add_argument("--dst_mac", "-d", dest="dst_mac", help="Destination MAC address to use in the generated PCAP file", default="11:11:11:11:11:11")
    args = parser.parse_args()
    dst_mac = args.dst_mac
    gen_pcap_with_md_portknock(args.num_cores, dst_mac, args.output_path, args.input_file)
