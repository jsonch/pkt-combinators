#include <stdlib.h> 
#include <sys/mman.h>
#include <stdlib.h> // For exit
#include <stdio.h> // For perror
#include <rte_lcore.h> // For LCore

/*
parse packet, count, print, and reflect packets
*/

#define PYTHON(x)


// parse atom 
PYTHON({
parse_atom = atom(
    state_init=None,
    state_ty=None,
    f="parse", 
    arg_tys=[], # NOTE: the packet arg isnt declared
    ret_ty="hdr_t")    
})
typedef struct {
    int a;
    int b;
} hdr_t;

// NOTE: all atoms must have a state and packet arg, even if they arent used
// NOTE: all pipeline variables (e.g. hdr_t) must be passed by reference, even
// though they are declared as value types in the atom declaration
void parse(void  * nostate, char * pkt, hdr_t* hdr) {
    hdr_t* pkt_hdr = (hdr_t*) pkt;
    hdr->a = pkt_hdr->a;
    hdr->b = pkt_hdr->b;
    return;
}
// counter atom
PYTHON({
counter_atom = atom(
    state_init="counter_init",
    state_ty="int",
    f="counter", 
    arg_tys=[],
    ret_ty="int")    
    })
int* counter_init() {
    int* counter = mmap(NULL, sizeof(int), PROT_READ | PROT_WRITE, MAP_SHARED | MAP_ANONYMOUS, -1, 0);
    *counter = 0;
    return counter;
}
void counter(int* ct, char * pkt, int* ct_val) {
    (*ct)++;
    *ct_val = *ct;
}

// print atom
PYTHON({
print_atom = atom(
    state_init=None,
    state_ty=None,
    f="print_pkt", 
    arg_tys=["hdr_t", "int"],
    ret_ty=None)    
})

void print_pkt(void * nostate, char * pkt, hdr_t* hdr, int* ct) {
    printf("packet number %d: a=%d b=%d\n", *ct, hdr->a, hdr->b);
}

// pipeline declaration
PYTHON({
device0 = (0, 0) # format: (<internal dpdk nic number, nic queue>)

c0_pipe=\
at("c0",
    let("hdr", do(parse_atom, []),
    at("c1", 
        let("cur_ct", do(counter_atom, []),
        seq(
            do(print_atom, ["hdr", "cur_ct"]),
            forward(device0))))))

full_pipe = nic([(device0, c0_pipe)])
print(compile_dpdk(full_pipe))
})