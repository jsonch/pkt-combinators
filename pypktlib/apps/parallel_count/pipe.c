
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

typedef struct counter_state_t {
    uint32_t* counter;
} counter_state_t;


typedef struct eth_t {
    uint8_t  dst[6];   // Destination MAC address
    uint8_t  src[6];    // Source MAC address
    uint16_t type;      // EtherType
} __attribute__((packed)) eth_t;

typedef char* str_t;
/********** atom definitions ***********/

counter_state_t* count_init(int len){
    counter_state_t* state = malloc(sizeof(counter_state_t));
    state->counter = mmap(NULL, len * sizeof(uint32_t *), PROT_READ | PROT_WRITE, MAP_SHARED | MAP_ANONYMOUS, -1, 0);
    for (int i = 0; i < len; i++){
        state->counter[i] = 0;
    }
    return state;
}

void count(counter_state_t* state, char* pkt, uint32_t* count) {
    // update counter and return modulo 5
    (*state->counter)++;
    *state->counter %= 5;
    *count = *state->counter;
}

void parse_eth(void * nostate, char * pkt, eth_t* eth) {
    eth = (eth_t*) pkt;
}


void count(counter_state_t* state, char* pkt, uint32_t* count) {
        // update counter and return
        if (eht->dst[5] & 1 == 1) {
            (*state->counter)++;
            *count = *state->counter;
        }
    }

void print(void* nostate, char* pkt, char* str) {
    printf("%s", str);
}


void print_ct(void* nostate, char* pkt, uint32_t* ct) {
    printf("count: %d\n", *ct);
}

/********** pipeline context / metadata ***********/

typedef struct mario_ctx_t {
    eth_t eth_2;
    uint32_t ct_3;
    eth_t eth_4;
    uint32_t auto_shard_decision_name_1;
    uint32_t ct_5;
} mario_ctx_t;

/********** config ***********/

#define RX_RING_SIZE 1024
#define TX_RING_SIZE 1024
#define NUM_MBUFS 8191
#define MBUF_CACHE_SIZE 250
#define BURST_SIZE 64
#define CACHE_LINE_SIZE 64
#define RING_SIZE 1024
cfg_t cfg = {
	.rx_ring_size = RX_RING_SIZE,
	.tx_ring_size = TX_RING_SIZE,
	.num_mbufs = NUM_MBUFS,
	.mbuf_cache_size = MBUF_CACHE_SIZE,
    .metadata_size = sizeof(mario_ctx_t)
};    

/********** queue declarations ***********/

struct rte_ring* queue_9 = NULL;
struct rte_ring* queue_11 = NULL;

void init_queues(void) {
    queue_9 = rte_ring_create("queue_9", RING_SIZE, rte_socket_id(), RING_F_SP_ENQ | RING_F_SC_DEQ);
queue_11 = rte_ring_create("queue_11", RING_SIZE, rte_socket_id(), RING_F_SP_ENQ | RING_F_SC_DEQ);
}

/********** atom state  ***********/

counter_state_t * anon_state_2_instance_1;
void * _no_state = NULL;
counter_state_t * rr_count_state_0;

void init_state(void) {
        anon_state_2_instance_1 = count_init(1);
    
    rr_count_state_0 = count_init(2);
}

/********** segment functions ***********/


void run_segment_Core c0_segment_7(void) {
    struct rte_mbuf * mbuf = NULL;
    const uint16_t nb_rx = rte_eth_rx_burst(0, 0, &mbuf, 1);
    if (nb_rx > 0) {
    mario_ctx_t* ctx = rte_pktmbuf_mtod_offset(mbuf, mario_ctx_t *, rte_pktmbuf_pkt_len(mbuf));
    char * pkt = rte_pktmbuf_mtod(mbuf, char *);
            rr_count(rr_count_state_0, pkt, (uint32_t *)&(ctx->auto_shard_decision_name_1));
        switch (ctx->auto_shard_decision_name_1) {    
            case Core c1:{
                rte_ring_enqueue(queue_9, mbuf);
            }
            case Core c2:{
                rte_ring_enqueue(queue_11, mbuf);
            }
        }
    }
    return;    
}

void run_segment_Core c2_segment_10(void) {
    struct rte_mbuf * mbuf = NULL;
    if (rte_ring_dequeue(queue_11, (void **) &mbuf) == 0) {
    mario_ctx_t* ctx = rte_pktmbuf_mtod_offset(mbuf, mario_ctx_t *, rte_pktmbuf_pkt_len(mbuf));
    char * pkt = rte_pktmbuf_mtod(mbuf, char *);
            parse_eth(_no_state, pkt, (eth_t *)&(ctx->eth_4));
        countifethodd(anon_state_2_instance_1, pkt, (eth_t *)&(ctx->eth_4), (uint32_t *)&(ctx->ct_5));
        print(_no_state, pkt, "ip ");
        print_ct(_no_state, pkt, (uint32_t *)&(ctx->ct_5));
        const uint16_t nb_tx = rte_eth_tx_burst(0, 0, &mbuf, 1);
        if (nb_tx == 0) {rte_pktmbuf_free(mbuf);}
        return;
    }
    return;
}

void run_segment_Core c1_segment_8(void) {
    struct rte_mbuf * mbuf = NULL;
    if (rte_ring_dequeue(queue_9, (void **) &mbuf) == 0) {
    mario_ctx_t* ctx = rte_pktmbuf_mtod_offset(mbuf, mario_ctx_t *, rte_pktmbuf_pkt_len(mbuf));
    char * pkt = rte_pktmbuf_mtod(mbuf, char *);
            parse_eth(_no_state, pkt, (eth_t *)&(ctx->eth_2));
        countifethodd(anon_state_2_instance_1, pkt, (eth_t *)&(ctx->eth_2), (uint32_t *)&(ctx->ct_3));
        print(_no_state, pkt, "ip ");
        print_ct(_no_state, pkt, (uint32_t *)&(ctx->ct_3));
        const uint16_t nb_tx = rte_eth_tx_burst(0, 0, &mbuf, 1);
        if (nb_tx == 0) {rte_pktmbuf_free(mbuf);}
        return;
    }
    return;
}
/********** location processes ***********/

static int core_Core c0(void *arg __attribute__((unused))) {
    printf("Core %u running location Core c0\n", rte_lcore_id());
    while(1) {
            run_segment_Core c0_segment_7();
    }
    return 0;
}


static int core_Core c2(void *arg __attribute__((unused))) {
    printf("Core %u running location Core c2\n", rte_lcore_id());
    while(1) {
            run_segment_Core c2_segment_10();
    }
    return 0;
}


static int core_Core c1(void *arg __attribute__((unused))) {
    printf("Core %u running location Core c1\n", rte_lcore_id());
    while(1) {
            run_segment_Core c1_segment_8();
    }
    return 0;
}

static int (*location_processes[])(void *) = {
core_Core c0,
core_Core c2,
core_Core c1
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
    if (core_count < 3+1) {
        printf("Error: There are 3 location processes, but only %d cores.\n", core_count);
        rte_eal_cleanup();
        return -1;
    }
    uint32_t proc_idx = 0;
    uint32_t lcore_id;
    RTE_LCORE_FOREACH_WORKER(lcore_id) {
        if (proc_idx < 3) {
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
