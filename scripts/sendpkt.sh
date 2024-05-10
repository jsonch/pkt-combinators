usage="send a raw packet on an interface using socat. 
    Usage: $0 <interface> [raw_packet]
    <interface> is the interface to send the packet on
    [raw_packet] is the raw packet as a colon-delimeted hex string.
      Default: 00:01:02:03:04:05:06:07:08:09:0a:0b:08:00:00:00:00:00
"

if [ -z "$1" ]
then
    echo "$usage"
    exit 1
fi
if [ "$1" == "-h" ] || [ "$1" == "--help" ]
then
    echo "$usage"
    exit 0
fi
if=$1
# second input is the raw packet as a hex string, it is optional with a default
if [ -z "$2" ]
then
    raw_pkt="00:01:02:03:04:05:06:07:08:09:0a:0b:08:00:00:00:00:00"
    # raw_pkt="\x00\x01\x02\x03\x04\x05\xa2\xfd\xc0\x3d\xf7\xf4\x08\x00\x00\x00\x00\x00"
else
    raw_pkt=$2
fi

# construct a command to echo the raw packet to socat
mk_echo_cmd() {
    # format the raw packet from a colon delimeted hex string to a hex string with \x
    raw_pkt=$1
    raw_pkt=$(echo $raw_pkt | sed 's/:/\\x/g' | awk '{print "\\x"$0}')
    echo "echo -n -e '"$raw_pkt"'"
}

# construct a socat command to send a bytestring to a socket as a raw packet
mk_socat_cmd() {
    if=$1
    # derive ifindex and src_mac
    ifindex_int=`cat /sys/class/net/$if/ifindex`
    # convert ifindex to 4-byte hex string
    printf -v ifindex_hex "%08x" $ifindex_int
    # reverse byte order
    ifindex_hex=$(echo $ifindex_hex | sed 's/\(..\)/\1 /g' | tac -s' ' | tr -d ' ' | xargs)

    # prepare arguments to socket_sendto
    domain=17             # PF_PACKET
    type=3                  # SOCK_RAW
    protocol=0              

    # build up the sockaddr_ll struct
        protocol_inner=x0300 # htons(ETH_P_ALL)
        ifindex=x$ifindex_hex
        hatype=x0000
        pkttype=x00
        halen=x06 # address length. This is a mac addr so its 6 bytes
        addr=x0000000000000000 # address is because it doesnt matter, has to be 8 bytes long
    sockaddr_ll=$protocol_inner$ifindex$hatype$pkttype$halen$addr

    SOCAT_ARG="SOCKET-SENDTO:$domain:$type:$protocol:$sockaddr_ll"
    echo "sudo socat - $SOCAT_ARG"
}

cmd="$(mk_echo_cmd $raw_pkt) | $(mk_socat_cmd $if)"
# echo "executing: $cmd"
eval $cmd