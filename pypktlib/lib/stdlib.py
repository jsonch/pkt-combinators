"""
    A standard library of C helpers, 
    types, atoms, and maybe even some pipes and combinators?
"""
from names import *
from ty import *
from usersyntax import *

# some ethernet devices (dpdk dev id, dpdk port id)
eth0 = (0, 0)
eth1 = (1, 0)


### types
uint16_t = UserTy(
    name = "uint16_t"
)

uint32_t = UserTy(
    name = "uint32_t"
)

eth_t = UserTy(
    name = "eth_t",
    cstr = 
"""
typedef struct eth_t {
    uint8_t  dest[6];   // Destination MAC address
    uint8_t  src[6];    // Source MAC address
    uint16_t type;      // EtherType
} __attribute__((packed)) eth_t;
"""
)

ip_t = UserTy(
    name = "ip_t",
    cstr = 
"""
typedef struct ip_t {
    uint8_t  ver_ihl;   // Version (4 bits) + Internet header length (4 bits)
    uint8_t  tos;       // Type of service 
    uint16_t len;       // Total length 
    uint16_t id;        // Identification
    uint16_t off;       // Fragment offset field
    uint8_t  ttl;       // Time to live
    uint8_t  p;         // Protocol 
    uint16_t sum;       // Checksum
    uint32_t src;       // Source address
    uint32_t dest;      // Destination address
} __attribute__((packed)) ip_t;
"""
)

parse_eth = Atom[None, None,ref[eth_t]](
    name="parse_eth", 
    f="""
void parse_eth(void * nostate, char * pkt, eth_t* eth) {
    eth = (eth_t*) pkt;
}
"""
)()

get_eth_ty = Atom[None, ref[eth_t], ref[uint16_t]](
    name="get_eth_ty",
    f="""
void get_eth_ty(void * nostate, char * pkt, eth_t* eth, uint16_t ety)
    """
)()

counter_t = UserTy(
    name = "counter_t",
    cstr = "typedef uint32_t counter_t;"
)

count_ip = Atom[ref[counter_t], ref[uint16_t], ref[uint32_t]](
    name="count_ip",
    f="""
void count_ip(counter_t* ct, uint16_t* ety, uint32_t* count) {
    if (ety = 0x0800) {
        (*ct)++;
        *count = *ct;
    }
}
""",
    initname="count_init",
    init="""
counter_t* count_init(){
    counter_t* counter = mmap(NULL, sizeof(counter_t), PROT_READ | PROT_WRITE, MAP_SHARED | MAP_ANONYMOUS, -1, 0);
    *counter = 0;
    return counter;
}
"""
)()

print_ct = Atom[None, ref[uint32_t], None](
    name="print_ct",
    f="""
void print_ct(uint32_t* ct) {
    printf("count: %d\\n", *ct);
}
"""
)()
