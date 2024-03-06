/*
    compile: gcc -Wall -g -o portknock portknock.c -lpcap
    run: ./portknock <pcap file> 

    This program is a demonstration of how to write a simple 
    stateful firewall as a pipeline of packet-processing atoms.

    An atom (e.g., "parse", "update", and "action") is a combination of 
    a packet processing function with a fixed type signature, 
    and a global state variable that only the atom can access.

    Each atom has the same type ("atom"): it takes a pointer to a packet 
    buffer and a pointer to a metadata struct, and returns a
    flag indicating whether to continue processing the packet.

    The programmer (or code generator) defines the metadata 
    struct and the atoms.

    There is a single packet handler function ("handler") that, 
    for each packet: 
    1. initializes an empty metadata struct
    2. calls each atom in sequence, passing in the packet buffer
       and the metadata struct
    3. if any atom returns a "stop" flag, the packet handler 
       returns immediately. (Note that, the packet handler 
       probably should return the same flag as the last atom
       to execute; TODO.)

    The packet handler function is user-written, but it is 
    very simple and should be easy to auto-generate. 

    The packet handler function is just called in a packet 
    rx loop from main.
    
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


/* computation must happen inside of "atom" functions, 
   which takes 
   a pointer to a packet buffer, and a pointer to a metadata. 
   return of 0 indicates success, 1 indicates failure. */
typedef enum { STOP = 0, CONTINUE = 1 } atom_ret;
typedef atom_ret (*atom)(const u_char *, struct metadata *); 

/* the program must call "CHECK_ATOM" on each atom, to make sure
   its of the correct type. "CHECK_ATOM" is just a noop, but 
   if the atom is of the wrong type, the c compiler will complain.*/
#define CHECK_ATOM(atom_name) atom atom_name##_checked = atom_name



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


/*** user/compiler written code ***/

// the metadata struct
typedef struct metadata {
    uint32_t pkt_len;
    uint32_t src_ip;
    uint32_t dst_ip;
    uint16_t src_port;
    uint16_t dst_port;
    uint32_t cur_state;
} metadata;

// atoms can be "stateless", like parse, in which 
// case they don't access any global state.
atom_ret parse(const u_char *pkt, metadata *m) {
    // exit if packet is too small
    if (m->pkt_len < sizeof(struct ether_header) + sizeof(struct iphdr) + sizeof(struct tcphdr)) {
        return STOP; 
    }
    struct ether_header *eth = (struct ether_header *)pkt;
    // exit if packet is not IP
    if (eth->ether_type != htons(ETH_P_IP)) {
        return STOP;
    }
    struct iphdr *ip = (struct iphdr *)(pkt + sizeof(struct ether_header));
    // exit if packet is not TCP
    if (ip->protocol != IPPROTO_TCP) {
        return STOP;
    }
    struct tcphdr *tcp = (struct tcphdr *)(pkt + sizeof(struct ether_header) + sizeof(struct iphdr));
    // parsing success -- fill in metadata
    m->src_ip = ip->saddr;
    m->dst_ip = ip->daddr;
    m->src_port = ntohs(tcp->source);
    m->dst_port = ntohs(tcp->dest);
    return CONTINUE;
}
// check the atom's type signature.
CHECK_ATOM(parse);


// state transition atom
enum state {
  CLOSED_0 = 0,
  CLOSED_1,
  CLOSED_2,
  OPEN,
};
#define PORT_1 100
#define PORT_2 101
#define PORT_3 102

// an atom can be "stateful", like update_state, which 
// accesses a pointer to global state that only the 
// atom can access.
typedef struct array_elem {
    uint32_t state;
} array_elem;
CREATE_STATE(port_states, array_elem, 1024);
atom_ret update(const u_char *pkt, metadata *m) {
    // get pointer to current state for the port
    array_elem *value = GET_STATE(port_states, array_elem, 0);
    if (!value) {return STOP;} // null pointer -- stop processing.
    // update the state
    if (value->state == OPEN) {
        // no update necessary
    } else if (value->state == CLOSED_0 && m->dst_port == PORT_1) {
        value->state = CLOSED_1;
    } else if (value->state == CLOSED_1 && m->dst_port == PORT_2) {
        value->state = CLOSED_2;
    } else if (value->state == CLOSED_2 && m->dst_port == PORT_3) {
        value->state = OPEN;
    } else {
        value->state = CLOSED_0;
    }
    // fill in the metadata field for cur_state
    m->cur_state = value->state;
    return CONTINUE;        
}
CHECK_ATOM(update);

// action atom -- swap ethernet addresses
atom_ret action(const u_char *pkt, metadata *m) {
    // swap ethernet addresses
    if (m->pkt_len < sizeof(struct ether_header)) {
        return STOP;
    }
    struct ether_header *eth = (struct ether_header *)pkt;
    uint8_t tmp[6];
    memcpy(tmp, eth->ether_shost, 6);
    memcpy(eth->ether_shost, eth->ether_dhost, 6);
    memcpy(eth->ether_dhost, tmp, 6);
    return CONTINUE;
}


// helpers
void metadata_to_string(struct metadata *m, char *buf) {
    char *src_ip_str = malloc(16); ip_to_string(m->src_ip, src_ip_str);
    char *dst_ip_str = malloc(16); ip_to_string(m->dst_ip, dst_ip_str);
    sprintf(buf, "src_ip: %s, dst_ip: %s, src_port: %d, dst_port: %d, pkt_len: %d cur_state: %d", src_ip_str, dst_ip_str, m->src_port, m->dst_port, m->pkt_len, m->cur_state);
    free(src_ip_str); free(dst_ip_str);
}
void print_metadata(struct metadata *m) {
    char strbuf[128];
    metadata_to_string(m, strbuf);
    printf("%s\n", strbuf);
}



/* structure of handler 

1. parse function: 
    parse packet, return metadata struct that contains: 
    - src ip, dst ip, src port, dst port, and "continue" field
    - "continue" field is a boolean that indicates whether to continue processing the packet
      - it is set to false if any errors occur while processing the packet
2. state function: 
    take metadata struct and use it to update a state machine
    if the state machine ends up in the OPEN state, set continue to true, else false
3. action function:
    swap source and destination addresses in original packet (if continue is set to true)
*/

/* packet handling function. User-written, but barely. */
void handle_packet(struct pcap_pkthdr *header, const u_char *packet) {
    printf ("---initial packet---\n");
    print_tcp_pkt(packet, header->len);
    // allocate packet-local metadata
    metadata m = {0}; m.pkt_len = header->len;
    // parse the packet
    if (parse(packet, &m) == STOP) {return;}
    printf("--- parsing success ---\n");
    // update the state
    if (update(packet, &m) == STOP) {return;}
    printf("--- state update success ---\n");
    // apply the action 
    if(action(packet, &m) == STOP) {return;}
    printf("--- action success ---\n");
    // print the packet buffer
    printf ("---final packet---\n");
    print_tcp_pkt(packet, header->len);
    printf ("---final metadata---\n");
    print_metadata(&m);

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

pcap_t* get_pcap_handle(char arg) {
    char errbuf[PCAP_ERRBUF_SIZE];
    pcap_t *handle = pcap_open_offline(arg, errbuf);
    if (handle == NULL) {
        printf("Couldn't open pcap file %s: %s\n", arg, errbuf);
        return handle;
    }
}

const u_char* load_next_pcap_pkt(pcap_t *handle, struct pcap_pkthdr header) {
    return pcap_next(handle, &header);
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