# allocate 512 hugepages
echo 512 > /sys/kernel/mm/hugepages/hugepages-2048kB/nr_hugepages

# veth pairs arent necessary for dpdk testing anymore
# mk_veth_pair() {
#     v1=$1
#     v2=$2
#     echo "Creating veth pair: $v1 <--> $v2"
#     sudo ip link add $v1 type veth peer name $v2
#     sudo ip link set $v1 up
#     sudo ip link set $v2 up
#     sudo ip link set $v1 promisc on
#     sudo ip link set $v2 promisc on
#     sudo sysctl -w net.ipv6.conf.$v1.disable_ipv6=1
#     sudo sysctl -w net.ipv6.conf.$v2.disable_ipv6=1
# }
# # make some veth pairs
# mk_veth_pair veth0 veth1
# mk_veth_pair veth2 veth3
