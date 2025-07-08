
/******* compiler-generated includes ********/

#include <stdlib.h> 
#include <stdint.h>
#include <inttypes.h> 
#include <unistd.h> // for sleep
#include <stdatomic.h> // atomics
#include <sys/mman.h> // shared memory
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <xmmintrin.h> // For _mm_pause()
#include <rte_memcpy.h>
#include "dpdk_init.h"

/********** user c code ***********/

/********** user types ***********/

typedef struct eth_t {
    uint8_t  dst[6];   // Destination MAC address
    uint8_t  src[6];    // Source MAC address
    uint16_t type;      // EtherType
} __attribute__((packed)) eth_t;


typedef struct mod_state_t {
    uint32_t* counter;
} mod_state_t;


typedef struct intpair_t {
    uint32_t count;
    uint32_t mod;
} intpair_t;


typedef struct counter_state_t {
    uint32_t* counter;
} counter_state_t;

typedef char* str_t;
/********** atom definitions ***********/

void parse_eth(void * nostate, char * pkt, eth_t* eth) {
    *eth = *((eth_t*)pkt);
}


void get_eth_ty(void * nostate, char * pkt, eth_t* eth, uint16_t* ety) {
    *ety = eth->type;
}
    

mod_state_t* mod_init(int len){
    mod_state_t* state = malloc(sizeof(mod_state_t));
    state->counter = mmap(NULL, len * sizeof(uint32_t *), PROT_READ | PROT_WRITE, MAP_SHARED | MAP_ANONYMOUS, -1, 0);
    for (int i = 0; i < len; i++){
        state->counter[i] = 0;
    }
    return state;
}

    void counter_and_mod(mod_state_t* state, char* pkt, intpair_t* result) {
            // update mod and return
            (*state->counter)++;
            result->count = *state->counter;
            result->mod = *state->counter % 3;
        }
        

counter_state_t* count_init(int len){
    counter_state_t* state = malloc(sizeof(counter_state_t));
    state->counter = mmap(NULL, len * sizeof(uint32_t *), PROT_READ | PROT_WRITE, MAP_SHARED | MAP_ANONYMOUS, -1, 0);
    for (int i = 0; i < len; i++){
        state->counter[i] = 0;
    }
    return state;
}

void count(counter_state_t* state, char* pkt, uint32_t* count) {
        // update counter and return
        (*state->counter)++;
        *count = *state->counter;
    }

void print(void* nostate, char* pkt, char* str) {
    printf("%s", str);
}


void print_ct(void* nostate, char* pkt, uint32_t* ct) {
    printf("count: %d\n", *ct);
}

/********** pipeline context / metadata ***********/

typedef struct mario_ctx_t {
    uint32_t ct_6;
    uint16_t ety_1;
    uint32_t ct_5;
    uint32_t ct_3;
    eth_t eth_0;
    intpair_t result_2;
    uint32_t ct_4;
} mario_ctx_t;

/********** config ***********/

#define RX_RING_SIZE 1024
#define TX_RING_SIZE 1024
#define NUM_MBUFS 8191
#define MBUF_CACHE_SIZE 250
#define BURST_SIZE 64
#define CACHE_LINE_SIZE 64
#define RING_SIZE 1024
#define NUM_TX_QUEUES 8
cfg_t cfg = {
	.rx_ring_size = RX_RING_SIZE,
	.tx_ring_size = TX_RING_SIZE,
	.num_mbufs = NUM_MBUFS,
	.mbuf_cache_size = MBUF_CACHE_SIZE,
    .metadata_size = sizeof(mario_ctx_t),
    .num_tx_queues = NUM_TX_QUEUES
};    

/********** queue declarations ***********/

struct rte_ring* queue_10 = NULL;
struct rte_ring* queue_12 = NULL;
struct rte_ring* queue_14 = NULL;
struct rte_ring* queue_16 = NULL;

void init_queues(void) {
    queue_10 = rte_ring_create("queue_10", RING_SIZE, rte_socket_id(), RING_F_SP_ENQ | RING_F_SC_DEQ);
queue_12 = rte_ring_create("queue_12", RING_SIZE, rte_socket_id(), RING_F_SP_ENQ | RING_F_SC_DEQ);
queue_14 = rte_ring_create("queue_14", RING_SIZE, rte_socket_id(), RING_F_SP_ENQ | RING_F_SC_DEQ);
queue_16 = rte_ring_create("queue_16", RING_SIZE, rte_socket_id(), RING_F_SP_ENQ | RING_F_SC_DEQ);
}

/********** atom state  ***********/

void * _no_state = NULL;
mod_state_t * foo_instance_1;
counter_state_t * nonip_instance_1;
counter_state_t * ip_instance_1;

void init_state(void) {
        
    foo_instance_1 = mod_init(1);
    nonip_instance_1 = count_init(1);
    ip_instance_1 = count_init(1);
}

/********** segment functions ***********/

void run_segment_c0_segment_8(void) {
    struct rte_mbuf * mbufs[32];
    struct rte_mbuf * mbuf = NULL;
    const uint16_t nb_rx = rte_eth_rx_burst(0, 0, mbufs, 32);
    if (nb_rx > 0) {
        for (uint32_t i = 0; i < nb_rx; i++){
            mbuf = mbufs[i];
            mario_ctx_t* ctx = rte_pktmbuf_mtod_offset(mbuf, mario_ctx_t *, rte_pktmbuf_pkt_len(mbuf));
            char * pkt = rte_pktmbuf_mtod(mbuf, char *);
            parse_eth(_no_state, pkt, (eth_t *)&(ctx->eth_0));
            get_eth_ty(_no_state, pkt, (eth_t *)&(ctx->eth_0), (uint16_t *)&(ctx->ety_1));
            switch (ctx->ety_1) {    
                case 8:{
                    counter_and_mod(foo_instance_1, pkt, (intpair_t *)&(ctx->result_2));
                    switch (ctx->result_2.mod) {    
                        case 0:{
                            while (rte_ring_enqueue(queue_10, mbuf) != 0){}
                            continue;

                            break;
                        }
                        case 1:{
                            while (rte_ring_enqueue(queue_12, mbuf) != 0){}
                            continue;

                            break;
                        }
                        case 2:{
                            while (rte_ring_enqueue(queue_14, mbuf) != 0){}
                            continue;

                            break;
                        }
                    }
                    break;
                }
                default:{
                    while (rte_ring_enqueue(queue_16, mbuf) != 0){}
                    continue;

                }
            }
        }
    }
    return;
}
void run_segment_c4_segment_15(void) {
    struct rte_mbuf * mbuf = NULL;
    if (rte_ring_dequeue(queue_16, (void **) &mbuf) == 0) {
        mario_ctx_t* ctx = rte_pktmbuf_mtod_offset(mbuf, mario_ctx_t *, rte_pktmbuf_pkt_len(mbuf));
        char * pkt = rte_pktmbuf_mtod(mbuf, char *);
        count(nonip_instance_1, pkt, (uint32_t *)&(ctx->ct_6));
        print(_no_state, pkt, "non-ip ");
        print_ct(_no_state, pkt, (uint32_t *)&(ctx->ct_6));
        while (1 != rte_eth_tx_burst(0, 3, &mbuf, 1)) {}
        return;
    }
    return;
}
void run_segment_c3_segment_13(void) {
    struct rte_mbuf * mbuf = NULL;
    if (rte_ring_dequeue(queue_14, (void **) &mbuf) == 0) {
        mario_ctx_t* ctx = rte_pktmbuf_mtod_offset(mbuf, mario_ctx_t *, rte_pktmbuf_pkt_len(mbuf));
        char * pkt = rte_pktmbuf_mtod(mbuf, char *);
        count(ip_instance_1, pkt, (uint32_t *)&(ctx->ct_5));
        print(_no_state, pkt, "ip ");
        print_ct(_no_state, pkt, (uint32_t *)&(ctx->ct_5));
        while (1 != rte_eth_tx_burst(0, 2, &mbuf, 1)) {}
        return;
    }
    return;
}
void run_segment_c2_segment_11(void) {
    struct rte_mbuf * mbuf = NULL;
    if (rte_ring_dequeue(queue_12, (void **) &mbuf) == 0) {
        mario_ctx_t* ctx = rte_pktmbuf_mtod_offset(mbuf, mario_ctx_t *, rte_pktmbuf_pkt_len(mbuf));
        char * pkt = rte_pktmbuf_mtod(mbuf, char *);
        count(ip_instance_1, pkt, (uint32_t *)&(ctx->ct_4));
        print(_no_state, pkt, "ip ");
        print_ct(_no_state, pkt, (uint32_t *)&(ctx->ct_4));
        while (1 != rte_eth_tx_burst(0, 1, &mbuf, 1)) {}
        return;
    }
    return;
}
void run_segment_c1_segment_9(void) {
    struct rte_mbuf * mbuf = NULL;
    if (rte_ring_dequeue(queue_10, (void **) &mbuf) == 0) {
        mario_ctx_t* ctx = rte_pktmbuf_mtod_offset(mbuf, mario_ctx_t *, rte_pktmbuf_pkt_len(mbuf));
        char * pkt = rte_pktmbuf_mtod(mbuf, char *);
        count(ip_instance_1, pkt, (uint32_t *)&(ctx->ct_3));
        print(_no_state, pkt, "ip ");
        print_ct(_no_state, pkt, (uint32_t *)&(ctx->ct_3));
        while (1 != rte_eth_tx_burst(0, 0, &mbuf, 1)) {}
        return;
    }
    return;
}
/********** location processes ***********/

static int core_c0(void *arg __attribute__((unused))) {
    printf("Core %u running location c0\n", rte_lcore_id());
    while(1) {
            run_segment_c0_segment_8();
    }
    return 0;
}


static int core_c4(void *arg __attribute__((unused))) {
    printf("Core %u running location c4\n", rte_lcore_id());
    while(1) {
            run_segment_c4_segment_15();
    }
    return 0;
}


static int core_c3(void *arg __attribute__((unused))) {
    printf("Core %u running location c3\n", rte_lcore_id());
    while(1) {
            run_segment_c3_segment_13();
    }
    return 0;
}


static int core_c2(void *arg __attribute__((unused))) {
    printf("Core %u running location c2\n", rte_lcore_id());
    while(1) {
            run_segment_c2_segment_11();
    }
    return 0;
}


static int core_c1(void *arg __attribute__((unused))) {
    printf("Core %u running location c1\n", rte_lcore_id());
    while(1) {
            run_segment_c1_segment_9();
    }
    return 0;
}

static int (*location_processes[])(void *) = {
core_c0,
core_c4,
core_c3,
core_c2,
core_c1
};
/********** main ***********/

int main(int argc, char *argv[])
{	
    dpdk_init(cfg, argc, argv); // init dpdk, ports, and memory pools
    printf("DPDK initialized\n");
    init_state(); // init atom states
    init_queues(); // init queues
    // get all the lcores
    int core_count = rte_lcore_count();
    // one extra core for main
    if (core_count < 5+1) {
        printf("Error: There are 5 location processes, but only %d cores.\n", core_count);
        rte_eal_cleanup();
        return -1;
    }
    uint32_t proc_idx = 0;
    uint32_t lcore_id;
    RTE_LCORE_FOREACH_WORKER(lcore_id) {
        if (proc_idx < 5) {
            rte_eal_remote_launch(location_processes[proc_idx], NULL, lcore_id);
            proc_idx++;
        }
    }
    printf("\nMain Core %u waiting on workers. [Ctrl+C to quit]\n", rte_lcore_id());
    while(1) {
        sleep(1);
    }
    rte_eal_cleanup();
    return 0;
}
