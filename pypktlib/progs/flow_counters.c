#include <stdlib.h> 
#include <stdint.h>
#include <sys/mman.h>
#include <stdlib.h> // For exit
#include <stdio.h> // For perror
#include <rte_lcore.h> // For LCore

/*
    Parse ethernet header and count the number of packets with each mac address. 
    Print current packet and current counts. 
*/

#define PYTHON(x)


typedef struct eth_t {
    uint8_t  dest[6];   // Destination MAC address
    uint8_t  src[6];    // Source MAC address
    uint16_t type;      // EtherType
} __attribute__((packed)) eth_t;

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


// parse atom 
PYTHON({
get_eth_ty = atom(
    state_init=None,
    state_ty=None,
    f="get_eth_ty_f", 
    arg_tys=[], # NOTE: the packet arg isnt declared
    ret_ty="uint16_t")    
})


void get_eth_ty_f(void * nostate, char * pkt, uint16_t* eth_ty) {
    eth_t* eth = (eth_t*) pkt;
    // convert to little-endian
    *eth_ty = (eth->type >> 8) | (eth->type << 8);
}

// print atom
PYTHON({
print_pkt = atom(
    state_init=None,
    state_ty=None,
    f="print_pkt_f", 
    arg_tys=["uint16_t", "int"],
    ret_ty=None)    
})

void print_pkt_f(void * nostate, char * pkt, uint16_t* eth_ty, int* counter_val) {
    eth_t* eth = (eth_t*) pkt;
    ip_t* ip = (ip_t*) (pkt + sizeof(eth_t));
    printf("packet[%i]\nraw eth: dest=%02x:%02x:%02x:%02x:%02x:%02x src=%02x:%02x:%02x:%02x:%02x:%02x type=%04x\n",
        *counter_val,
        eth->dest[0], eth->dest[1], eth->dest[2], eth->dest[3], eth->dest[4], eth->dest[5],
        eth->src[0], eth->src[1], eth->src[2], eth->src[3], eth->src[4], eth->src[5],
        eth->type);
    printf("ip: src=%d dest=%d\n", ip->src, ip->dest);
    printf("parsed eth type: %d\n", *eth_ty);
}

PYTHON({
count = atom(
    state_init="init_int",
    state_ty="int",
    f="count_f", 
    arg_tys=[],
    ret_ty="int")
})

int* init_int() {
    int* counter = mmap(NULL, sizeof(int), PROT_READ | PROT_WRITE, MAP_SHARED | MAP_ANONYMOUS, -1, 0);
    *counter = 0;
    return counter;
}

void count_f(int* counter, char * pkt, int* counter_val) {
    (*counter)++;
    *counter_val = *counter;
}



// pipeline declaration
PYTHON({
device0 = (0, 0) # format: (<internal dpdk nic number, nic queue>)

c0_pipe = at("c0",
            let("eth_ty", do(get_eth_ty, []),
            let("cur_ct", do(count, []),
            seq(
                do(print_pkt, ["eth_ty", "cur_ct"]),
                forward(device0)))))

full_pipe = nic([(device0, c0_pipe)])
print(compile_dpdk(full_pipe))
})