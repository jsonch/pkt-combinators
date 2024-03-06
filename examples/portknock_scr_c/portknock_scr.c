// round-robin scheduling with shared nothing (State-Compute Replication)
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>           // for shared memory
#include <unistd.h>             // for fork
#include <pcap.h>              // for pcap functions
#include <sys/wait.h>          // for wait

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

typedef uint8_t u_char;

/* user-defined stuff */

// user-defined metadata
struct metadata; 

// user-defined atom type
typedef enum { STOP = 0, CONTINUE = 1 } atom_ret;
typedef atom_ret (*atom)(const u_char *, struct metadata *); 

// user-defined packet handler
void handle_packet(struct pcap_pkthdr *header, const u_char *packet);


/* helper functions for user code */
void ip_to_string(uint32_t ip, char *buf);
void print_tcp_pkt(const u_char *pkt, int len);


/* state macro helpers for user code */

/* STATE_DECLARE_NONSHARED(state_name, state_type, state_size, init_value)
    This sets up a state variable that is shared across processes.
    It should be called in the each worker (thread) scope. It declares:
    1. a struct type for the state variable 
    2. an instance of the state variable
    3. a function to allocate a state variable
    i.e.: 
        typedef struct {
            $(state_type) cells[$(state_size)];
        } $(state_name)_t;

        $(state_name)_t *create_shared_data() {
            $(state_name)_t out_var;
            // ... mmap stuff elided ...
            // for each cell i in out_var -> cells:
            //  out_var[i] = init_value            
            // return out_var;        }
*/        
#define STATE_DECLARE_NONSHARED(state_name, state_type, state_size, init_value) \
    typedef struct { \
        state_type cells[state_size]; \
    } state_name##_t; \
    \
    state_name##_t *state_name; \
    \
    state_name##_t *create_##state_name() { \
        state_name##_t *out_var = mmap(NULL, sizeof(state_name##_t), PROT_READ | PROT_WRITE, MAP_SHARED | MAP_ANONYMOUS, -1, 0); \
        if (out_var == MAP_FAILED) { \
            perror("mmap failed"); \
            return NULL; \
        } \
        for (int i = 0; i < state_size; i++) { \
            out_var->cells[i] = init_value; \
        } \
        \
        return out_var; \
    }

/* STATE_ALLOC(state_name)
    Call this to allocate an instance of the state 
    that can be shared across processes. Probably call 
    this in main. 
    It initializes the global state variable $state_name by calling: 
    $(state_name) = create_$(state_name)();
*/
#define STATE_ALLOC(state_name) \
    state_name = create_##state_name();

/* STATE_GET(state_name, index) 
    Call this to get a _pointer_ to the contents 
    of the given state variable at the given index. 
    i.e., &( $(state_name)->cells[$(index)] )
*/
#define STATE_GET(state_name, index) \
    &(state_name->cells[index])


/*** user written code ***/

// pkt_info struct
// packet fields that are related to state
typedef struct pkt_info_t {
    uint32_t src_ip;
    uint32_t dst_ip;
    uint16_t src_port;
    uint16_t dst_port;
} pkt_info_t;

// the metadata struct
#ifndef NUM_CORES
    #define NUM_CORES 1
#endif
typedef struct metadata {
    pkt_info_t pkt_info[NUM_CORES];
    uint32_t pkt_len;
    uint32_t cur_state;
} metadata;

// helpers
void pkt_info_to_string(struct pkt_info_t *info, char *buf) {
    char *src_ip_str = malloc(16); ip_to_string(info->src_ip, src_ip_str);
    char *dst_ip_str = malloc(16); ip_to_string(info->dst_ip, dst_ip_str);
    sprintf(buf, "src_ip: %s, dst_ip: %s, src_port: %d, dst_port: %d", src_ip_str, dst_ip_str, info->src_port, info->dst_port);
    free(src_ip_str); free(dst_ip_str);
}
void print_metadata(struct metadata *m) {
    char strbuf[128];
    for (int i = 0; i < NUM_CORES; i++) {
        pkt_info_to_string(&m->pkt_info[i], strbuf);
        printf("%d: %s\n", i, strbuf);
    }
    printf("pkt len: %d, cur_state: %d\n", m->pkt_len, m->cur_state);
}

atom_ret parse_history(const u_char *pkt, metadata* m) {
    // exit if packet is too small
    uint32_t history_size = sizeof(struct pkt_info_t) * (NUM_CORES - 1);
    if (m->pkt_len < sizeof(struct ether_header) + history_size) {
        return STOP; 
    }
    // copy packet history from packet to metadata.pkt_info[0, N-2]
    const u_char* pkt_info_start = pkt + sizeof(struct ether_header);
    memcpy(m->pkt_info, pkt_info_start, history_size);
    return CONTINUE;
}

/** atoms and state **/
atom_ret parse(const u_char *pkt, uint32_t pkt_len, pkt_info_t *info) {
    // exit if packet is too small
    if (pkt_len < sizeof(struct ether_header) + sizeof(struct iphdr) + sizeof(struct tcphdr)) {
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
    info->src_ip = ip->saddr;
    info->dst_ip = ip->daddr;
    info->src_port = ntohs(tcp->source);
    info->dst_port = ntohs(tcp->dest);
    return CONTINUE;
}

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
// state machine for port knocking -- slightly different 
// macro from shared version. Ideally, 
// we'd just call "STATE_DECLARE_NONSHARED" with the same args?
// We don't need locks in this function as private state won't
// be updated by other cores
STATE_DECLARE_NONSHARED(port_states, int, 1024, 0)
atom_ret state_transition(const u_char *pkt, pkt_info_t *m, uint32_t *state) {
    // get pointer to current state for the port
    // We might also need to provide the core/processor id?
    int *value = STATE_GET(port_states, 0);
    if (!value) {return STOP;} // null pointer -- stop processing.
    // update the state
    if (*value == OPEN) {
        // no update necessary
    } else if (*value == CLOSED_0 && m->dst_port == PORT_1) {
        *value = CLOSED_1;
    } else if (*value == CLOSED_1 && m->dst_port == PORT_2) {
        *value = CLOSED_2;
    } else if (*value == CLOSED_2 && m->dst_port == PORT_3) {
        *value = OPEN;
    } else {
        *value = CLOSED_0;
    }
    // update the state
    *state = *value;
    return CONTINUE;        
}

atom_ret update(const u_char *pkt, metadata *m) {
    for (int i = 0; i < NUM_CORES; i++) {
        printf("%d: update state from %d to ", i, m->cur_state);
        state_transition(pkt, &m->pkt_info[i], &m->cur_state);
        printf("%d\n", m->cur_state);
    }
    return CONTINUE;
}

// action atom -- swap ethernet addresses
// same as non-shared version
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


/* packet handling function. User-written, but barely. 
   packet format: | dummy ethernet header | packet history | original packet |
   packet history format: | pkt_info[i-N+1] | pkt_info[i-N+2] | ... |pkt_info[i-1]|
   `i` is the id of original packet
*/
void handle_packet(struct pcap_pkthdr *header, const u_char *packet) {
    printf ("---initial packet---\n");
    print_tcp_pkt(packet, header->len);
    // allocate packet-local metadata
    metadata m = {0}; m.pkt_len = header->len;
    // parse the packet history
    if (parse_history(packet, &m) == STOP) {return;}
    // parse the packet
    uint32_t offset = sizeof(struct ether_header) + sizeof(struct pkt_info_t) * (NUM_CORES - 1);
    if (parse(packet + offset, m.pkt_len - offset, &m.pkt_info[NUM_CORES - 1]) == STOP) {return;}
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

/*** library / system code (the same for every program) ***/

void ip_to_string(uint32_t ip, char *buf) {
    // use inet function to convert ip address uint32_t to string
    struct in_addr addr;
    addr.s_addr = ip;
    char *ip_str = inet_ntoa(addr);
    sprintf(buf, "%s", ip_str);
}

void mac_addr_to_string(u_char* mac_addr, char* buf) {
    sprintf(buf, "%02x:%02x:%02x:%02x:%02x:%02x",
            mac_addr[0], mac_addr[1], mac_addr[2], mac_addr[3], mac_addr[4], mac_addr[5]);    
}

// print the dummy ehthernet header, packet history
// and the current packet including ethernet addresses, ip addresses, 
// and ports
void print_tcp_pkt(const u_char *pkt, int len) {
    uint32_t offset = 0;
    // print dummy ethernet
    struct ether_header *eth = (struct ether_header *)pkt;
    char* src_mac = malloc(20);
    char* dst_mac = malloc(20);
    mac_addr_to_string(eth->ether_shost, src_mac);
    mac_addr_to_string(eth->ether_dhost, dst_mac);
    printf("[dummy ethernet] src_mac: %s, dst_mac: %s\n", src_mac, dst_mac);
    free(src_mac);
    free(dst_mac);
    offset += sizeof(struct ether_header);
    // print packet history
    printf("[pkt history] \n");
    pkt_info_t* pkt_info = (pkt_info_t*)(pkt + offset);
    char* strbuf = malloc(128);
    for (int i = 0; i < NUM_CORES - 1; i++) {
        pkt_info_to_string(pkt_info, strbuf);
        printf("    %d: %s\n", i, strbuf);
        pkt_info += 1;
    }
    free(strbuf);
    offset += sizeof(struct pkt_info_t) * (NUM_CORES - 1);
    // print packet
    eth = (struct ether_header *)(pkt + offset);
    offset += sizeof(struct ether_header);
    struct iphdr *ip = (struct iphdr *)(pkt + offset);
    offset += sizeof(struct iphdr);
    struct tcphdr *tcp = (struct tcphdr *)(pkt + offset);
    char *src_ip_str = malloc(16); ip_to_string(ip->saddr, src_ip_str);
    char *dst_ip_str = malloc(16); ip_to_string(ip->daddr, dst_ip_str);
    printf("[current pkt] src_mac: %02x:%02x:%02x:%02x:%02x:%02x, dst_mac: %02x:%02x:%02x:%02x:%02x:%02x, src_ip: %s, dst_ip: %s, src_port: %d, dst_port: %d\n", 
        eth->ether_shost[0], eth->ether_shost[1], eth->ether_shost[2], eth->ether_shost[3], eth->ether_shost[4], eth->ether_shost[5],
        eth->ether_dhost[0], eth->ether_dhost[1], eth->ether_dhost[2], eth->ether_dhost[3], eth->ether_dhost[4], eth->ether_dhost[5],
        src_ip_str, dst_ip_str, ntohs(tcp->source), ntohs(tcp->dest));
    free(src_ip_str); free(dst_ip_str);
}

/*** main processing loop code -- same for every program ***/

/* toplevel control flow summary: 
    1. program takes a list of n pcaps as arguments, each pcap represents a single thread's workload from a single stream
    2. program allocates shared memory areas for state
    3. program forks n processes
    4. each process i runs the packet handler loop on the ith process */


// each worker thread / process runs this function, 
// which just opens its assigned pcap and runs the packet handler loop
void worker_main(char* pcap_fn) {
    printf("Child process created for pcap: %s\n", pcap_fn);
    STATE_ALLOC(port_states);

    char errbuf[PCAP_ERRBUF_SIZE];
    pcap_t *handle = pcap_open_offline(pcap_fn, errbuf);
    if (handle == NULL) {
        printf("Couldn't open pcap file %s: %s\n", pcap_fn, errbuf);
        return;
    }
    struct pcap_pkthdr header;
    const u_char *packet;
    while ((packet = pcap_next(handle, &header)) != NULL) {
        handle_packet(&header, packet);
    }

    pcap_close(handle);
    return;
}


// create worker processes, where each process i
// runs worker_main on the ith pcap file
int create_worker_processes(int num_processes, char *pcap_fns[]) {
    int i;
    for (i = 0; i < num_processes; i++) {
        pid_t pid = fork();

        if (pid < 0) {
            // Fork failed
            perror("fork failed");
            break;
        } else if (pid == 0) {
            // This is the child process
            // run the worker function, then exit
            worker_main(pcap_fns[i]);
            _exit(0);
        }
    }
    return i; // return the number of successfully created worker processes
}

int main(int argc, char *argv[]) {
    uint32_t num_threads = argc - 1;
    char **pcap_fns = argv + 1;
    printf("number of cores: %d\n", NUM_CORES);
    printf("number of threads / input pcaps: %d\n", num_threads);
    for(int i = 0; i < num_threads; i++) {
        printf("Thread %d input: %s\n", i, pcap_fns[i]);
    }
    // 1. spawn worker processes
    int num_created = create_worker_processes(num_threads, pcap_fns);
    // 2. wait for all worker processes to finish
    for(int i = 0; i < num_created; i++) {
        wait(NULL);
    }

    return 0;    

}