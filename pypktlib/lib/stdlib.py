"""
    A standard library of C helpers, 
    types, atoms, and maybe even some pipes and combinators?
"""
from .usersyntax import *


# some ethernet devices (dpdk dev id, dpdk port id)
eth0 = (0, 0)
eth1 = (1, 0)


### types
void_t = UserTy(
    name = "void"
)
uint8_t = UserTy(
    name = "uint8_t"
)
macaddr_t = UserTy(
    name = "macaddr_t",
    cstr = "typedef uint8_t macaddr_t[6];"
)


uint16_t = UserTy(
    name = "uint16_t"
)

uint32_t = UserTy(
    name = "uint32_t"
)
str_t = UserTy(
    name = "str_t",
    cstr = "typedef char* str_t;"
)

eth_t = UserTy(
    name = "eth_t",
    cstr = 
"""
typedef struct eth_t {
    uint8_t  dst[6];   // Destination MAC address
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

# get ingress port. TODO
get_ingress_port = Atom[None, None, ref[uint16_t]](
    name="get_ingress_port",
    f="""
void get_ingress_port(void * nostate, char * pkt, uint16_t* port) {
    // Logic to get ingress port from packet
    *port = 1; // Replace with actual logic
}
"""
)

# get dst mac directly from pointer to start of eth pkt
extract_dmac = Atom[None, None, ref[macaddr_t]](
    name="extract_dmac",
    f="""
void extract_dmac(void * nostate, char * pkt, macaddr_t* dmac) {
    memcpy(*dmac, pkt, 6);
}
"""
)

# get src mac directly from pointer to start of eth pkt
extract_smac = Atom[None, None, ref[macaddr_t]](
    name="extract_smac",
    f="""
void extract_smac(void * nostate, char * pkt, macaddr_t* smac) {
    memcpy(*smac, pkt + 6, 6);
}
"""
)

# get eth type directly from pointer to start of eth pkt
extract_etype = Atom[None, None, ref[uint16_t]](
    name="extract_etype",
    f="""
void extract_etype(void * nostate, char * pkt, uint16_t* etype) {
    *etype = *(uint16_t*)(pkt + 12);
}
"""
)

# parse full eth header from eth pkt
parse_eth = Atom[None, None,ref[eth_t]](
    name="parse_eth", 
    f="""
void parse_eth(void * nostate, char * pkt, eth_t* eth) {
    eth = (eth_t*) pkt;
}
"""
)

get_eth_ty = Atom[None, ref[eth_t], ref[uint16_t]](
    name="get_eth_ty",
    f="""
void get_eth_ty(void * nostate, char * pkt, eth_t* eth, uint16_t* ety) {
    *ety = eth->type;
}
    """
)

counter_state = AtomState(
    ty = ref[uint32_t],
    init = "count_init",
    cstr = """
uint32_t* count_init(int len){
    uint32_t * counter = mmap(NULL, len * sizeof(uint32_t *), PROT_READ | PROT_WRITE, MAP_SHARED | MAP_ANONYMOUS, -1, 0);
    for (int i = 0; i < len; i++){
        counter[i] = 0;
    }
    return counter;
}
""")

counter = Atom[counter_state(8), None, ref[uint32_t]](
    name="count",
    f="""
void count(uint32_t* ct, char* pkt, uint32_t* count) {
        // update counter and return
        (*ct)++;
        *count = *ct;
    }
""")



print_ct = Atom[None, ref[uint32_t], None](
    name="print_ct",
    f="""
void print_ct(void* nostate, char* pkt, uint32_t* ct) {
    printf("count: %d\\n", *ct);
}
"""
)

print_str = Atom[None, str_t, None](
    name="print",
    f="""
void print(void* nostate, char* pkt, char* str) {
    printf("%s", str);
}
"""
)
