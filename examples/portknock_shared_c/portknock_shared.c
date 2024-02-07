// round-robin scheduling with shared state + semaphores
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>           // for shared memory
#include <unistd.h>             // for fork
#include <pcap.h>              // for pcap functions
#include <semaphore.h>         // for semaphores

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

/* STATE_DECLARE_SHARED(state_name, state_type, state_size, init_value)
    This sets up a state variable that is shared across processes.
    It should be called in the global scope. It declares:
    1. a struct type for the state variable 
    2. a global instance of the state variable
    2. a function to allocate a shared state variable
    i.e.: 
        typedef struct {
            sem_t* lock;
            $(state_type) cells[$(state_size)];
        } $(state_name)_t;

        $(state_name)_t *create_shared_data() {
            $(state_name)_t out_var;
            // ... mmap stuff elided ...
            // for each cell i in out_var -> cells:
            //  out_var[i] = init_value            
            // return out_var;        }
*/        
#define STATE_DECLARE_SHARED(state_name, state_type, state_size, init_value) \
    typedef struct { \
        sem_t* lock; \
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
        \
        char sem_name[20]; \
        sprintf(sem_name, "/semaphore-%d", getpid()); \
        out_var->lock = sem_open(sem_name, O_CREAT, 0644, 1); \
        if (out_var->lock == SEM_FAILED) { \
            perror("sem_open failed"); \
            return NULL; \
        } \
        sem_unlink(sem_name); \
        \
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

/* STATE_LOCK(state_name)
    Call this to lock the shared state's semaphore. 
    Should be used before STATE_GET.
    sem_wait($(state_name)->lock);
*/
#define STATE_LOCK(state_name) \
    sem_wait(state_name->lock);

/* STATE_UNLOCK(state_name) 
    Call this to release the shared state's semaphore. 
    Should be used after you're finished modifying the state.
*/
#define STATE_UNLOCK(state_name) \
    sem_post(state_name->lock);



/*** user written code ***/

// the metadata struct
typedef struct metadata {
    uint32_t pkt_len;
    uint32_t src_ip;
    uint32_t dst_ip;
    uint16_t src_port;
    uint16_t dst_port;
    uint32_t cur_state;
} metadata;

STATE_DECLARE_SHARED(my_arr, int, 1024, 0)


/* packet handling function. User-written, but barely. */
void handle_packet(struct pcap_pkthdr *header, const u_char *packet) {
    printf ("---packet---\n");
    // Wait on the semaphore to ensure exclusive access to the shared state
    STATE_LOCK(my_arr);
    // Safely increment the shared state
    int* cell = STATE_GET(my_arr, 0);
    *cell += 1;
    printf("new value of my_arr[0] = %d\n", *cell);
    // unlock the semaphore
    STATE_UNLOCK(my_arr);

}

/*** library / system code (the same for every program) ***/

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
// runs worker_main on the ith pcap file and a pointer to the shared state
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
    printf("number of threads / input pcaps: %d\n", num_threads);
    for(int i = 0; i < num_threads; i++) {
        printf("Thread %d input: %s\n", i, pcap_fns[i]);
    }
    // 1. allocate shared memory + semaphore
    STATE_ALLOC(my_arr);
    // 2. spawn worker processes
    int num_created = create_worker_processes(num_threads, pcap_fns);
    // 3. wait for all worker processes to finish
    for(int i = 0; i < num_created; i++) {
        wait(NULL);
    }

    return 0;    

}