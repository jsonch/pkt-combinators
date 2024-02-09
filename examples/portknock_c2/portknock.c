/*
    compile: gcc -Wall -g -o portknock portknock.c -lpcap
    run: ./portknock <pcap file> 

    portknock_c with atoms with value args / returns 
    instead of pointers.
*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
// #include <netinet/if_ether.h>  // for struct ether_header (Ethernet header)
// #include <netinet/ip.h>        // for struct iphdr (IP header)
// #include <netinet/tcp.h>       // for struct tcphdr (TCP header)

/* inline header structs (for macos compatability) */
#define ETH_P_IP    0x0800      /* Internet Protocol packet */
#define ETHER_ADDR_LEN  6   /* length of an Ethernet address */
struct  ether_header {
    u_int8_t  ether_dhost[ETHER_ADDR_LEN];
    u_int8_t  ether_shost[ETHER_ADDR_LEN];
    u_int16_t ether_type;
} __attribute__((__packed__));

struct iphdr {
    uint8_t  ihl    :4,
         version:4;
    uint8_t   tos;
    uint16_t  tot_len;
    uint16_t  id;
    uint16_t  frag_off;
    uint8_t   ttl;
    uint8_t   protocol;
    uint16_t  check;
    int32_t   saddr;
    int32_t   daddr;
};
typedef        u_int32_t tcp_seq;

struct tcphdr {
    uint16_t  source;
    uint16_t  dest;
    uint32_t  seq;
    uint32_t  ack_seq;
    uint16_t   res1:4,
        doff:4,
        fin:1,
        syn:1,
        rst:1,
        psh:1,
        ack:1,
        urg:1,
        ece:1,
        cwr:1;
    uint16_t  window;
    uint16_t check;
    uint16_t  urg_ptr;
};


#include <pcap.h>              // for pcap functions


/*** static definitions used to structure user-written code ***/

/* the program must define a "metadata" struct, 
   which contains the data extracted from the packet.*/
struct metadata; 


/*** static library functions ***/
int main(int argc, char *argv[]);
void handle_packet(struct pcap_pkthdr *header, const u_char *packet);
void ip_to_string(uint32_t ip, char *buf);
void print_tcp_pkt(const u_char *pkt, int len);

// global state helpers
#define CREATE_STATE(state_name, state_type, state_size) \
    state_type state_name[state_size] = {0}
#define GET_STATE(state_name, state_type, index) \
    &((state_type *)state_name)[index]


// there are globals for the current packet and its length 
// an atom may modify cur_pkt as they please, but
// should not modify pkt_len...
// TODO: would this work in ebpf?
const u_char *pkt;
uint32_t pkt_len;

/*** user/compiler written code ***/

// atom structures
typedef struct flow_key_t {
    uint8_t  valid; // does every atom io type have to have a valid field? If not, how do we handle errors? 
    uint32_t src_ip;
    uint32_t dst_ip;
    uint16_t src_port;
    uint16_t dst_port;
} flow_key_t;


// parse extracts the 
flow_key_t parse() {
    flow_key_t rv = {0};
    // exit if packet is too small
    if (pkt_len < sizeof(struct ether_header) + sizeof(struct iphdr) + sizeof(struct tcphdr)) {
        return rv;
    }
    struct ether_header *eth = (struct ether_header *)pkt;
    // exit if packet is not IP
    if (eth->ether_type != htons(ETH_P_IP)) {
        return rv;
    }
    struct iphdr *ip = (struct iphdr *)(pkt + sizeof(struct ether_header));
    // exit if packet is not TCP
    if (ip->protocol != IPPROTO_TCP) {
        return rv;
    }
    struct tcphdr *tcp = (struct tcphdr *)(pkt + sizeof(struct ether_header) + sizeof(struct iphdr));
    // parsing success -- fill in metadata
    rv.src_ip = ip->saddr;
    rv.dst_ip = ip->daddr;
    rv.src_port = ntohs(tcp->source);
    rv.dst_port = ntohs(tcp->dest);
    rv.valid = 1;
    return rv;
}

// state transition atom
typedef enum {
  CLOSED_0 = 0,
  CLOSED_1,
  CLOSED_2,
  OPEN,
} state_t;
#define PORT_1 100
#define PORT_2 101
#define PORT_3 102

typedef struct {
    uint8_t valid; 
    state_t final_state;   
} update_result_t;


// state for update
typedef struct array_elem {
    uint32_t state;
} array_elem;

CREATE_STATE(port_states, array_elem, 1024);
update_result_t update(flow_key_t fk) {
    update_result_t rv  = {0};
    // get pointer to current state for the port    
    array_elem *value = GET_STATE(port_states, array_elem, 0);
    if (!value) {return rv;} // null pointer -- stop processing.
    // update the state
    if (value->state == OPEN) {
        // no update necessary
    } else if (value->state == CLOSED_0 && fk.dst_port == PORT_1) {
        value->state = CLOSED_1;
    } else if (value->state == CLOSED_1 && fk.dst_port == PORT_2) {
        value->state = CLOSED_2;
    } else if (value->state == CLOSED_2 && fk.dst_port == PORT_3) {
        value->state = OPEN;
    } else {
        value->state = CLOSED_0;
    }
    // fill in the metadata field for cur_state
    rv.valid = 1;
    rv.final_state = value->state;
    return rv;        
}

// action atom -- swap ethernet addresses
typedef enum {
  DROP = 0,
  PASS
} decision_t;

decision_t action(update_result_t res) {
    // swap ethernet addresses
    if (pkt_len < sizeof(struct ether_header)) {
        return DROP;
    }
    struct ether_header *eth = (struct ether_header *)pkt;
    // only swap if final state is OPEN
    if (res.final_state == OPEN) {
        uint8_t tmp[6];
        memcpy(tmp, eth->ether_shost, 6);
        memcpy(eth->ether_shost, eth->ether_dhost, 6);
        memcpy(eth->ether_dhost, tmp, 6);
    }
    return PASS;
}


/* packet handling function. just a chain of functions */
void handle_packet(struct pcap_pkthdr *header, const u_char *packet) {
    // set global packet and length
    pkt = packet;
    pkt_len = header->len;
    printf ("---initial packet---\n");
    print_tcp_pkt(packet, header->len);
    flow_key_t parse_out = parse();
    if (!parse_out.valid) {return;}
    update_result_t update_out = update(parse_out);
    if (!update_out.valid) {return;}
    decision_t action_out = action(update_out);
    printf("final packet---\n");
    print_tcp_pkt(packet, header->len);
    printf("final state: %d\n", update_out.final_state);
    printf("action: %d\n", action_out);
}


void ip_to_string(uint32_t ip, char *buf) {
    // use inet function to convert ip address uint32_t to string
    struct in_addr addr;
    addr.s_addr = ip;
    char *ip_str = inet_ntoa(addr);
    sprintf(buf, "%s", ip_str);
}

// print the ethernet addresses, ip addresses, and ports of a tcp packet
void print_tcp_pkt(const u_char *pkt, int len) {
    struct ether_header *eth = (struct ether_header *)pkt;
    struct iphdr *ip = (struct iphdr *)(pkt + sizeof(struct ether_header));
    struct tcphdr *tcp = (struct tcphdr *)(pkt + sizeof(struct ether_header) + sizeof(struct iphdr));
    char *src_ip_str = malloc(16); ip_to_string(ip->saddr, src_ip_str);
    char *dst_ip_str = malloc(16); ip_to_string(ip->daddr, dst_ip_str);
    printf("src_mac: %02x:%02x:%02x:%02x:%02x:%02x, dst_mac: %02x:%02x:%02x:%02x:%02x:%02x, src_ip: %s, dst_ip: %s, src_port: %d, dst_port: %d\n", 
        eth->ether_shost[0], eth->ether_shost[1], eth->ether_shost[2], eth->ether_shost[3], eth->ether_shost[4], eth->ether_shost[5],
        eth->ether_dhost[0], eth->ether_dhost[1], eth->ether_dhost[2], eth->ether_dhost[3], eth->ether_dhost[4], eth->ether_dhost[5],
        src_ip_str, dst_ip_str, ntohs(tcp->source), ntohs(tcp->dest));
    free(src_ip_str); free(dst_ip_str);
}



/* main loop -- open pcap, read files */
int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Usage: %s <filename>\n", argv[0]);
        return 1;
    }

    char errbuf[PCAP_ERRBUF_SIZE];
    pcap_t *handle = pcap_open_offline(argv[1], errbuf);
    if (handle == NULL) {
        printf("Couldn't open pcap file %s: %s\n", argv[1], errbuf);
        return 1;
    }

    struct pcap_pkthdr header;
    const u_char *packet;

    while ((packet = pcap_next(handle, &header)) != NULL) {
        handle_packet(&header, packet);
    }

    pcap_close(handle);
    return 0;
}