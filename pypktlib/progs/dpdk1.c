#include <stdlib.h> 
#include <sys/mman.h>
#include <stdlib.h> // For exit
#include <stdio.h> // For perror
#include <rte_lcore.h> // For LCore

/*
two instances of a packet counter on different cores with non-shared state, 
forward out of the input device (device id 0, which you bind to a device 
in dpdk startup args)
*/

#define PYTHON(x)

// counter atom
PYTHON({
counter_atom = atom(
    state_init="counter_init",
    state_ty="int",
    f="counter", 
    arg_tys=[],
    ret_ty=None)    
    })
int* counter_init() {
    int* counter = mmap(NULL, sizeof(int), PROT_READ | PROT_WRITE, MAP_SHARED | MAP_ANONYMOUS, -1, 0);
    *counter = 0;
    return counter;
}
void counter(int* ct, char * pkt) {
    (*ct)++;
    printf("non-shared counter on core: %d packet count: %d\n", rte_lcore_id(), *ct);
}

// pipeline declaration
PYTHON({
device0 = (0, 0) # format: (<internal dpdk nic number, nic queue>)

c0_pipe=at("c0",
    seq(
        do(counter_atom, []),
        at("c1", seq(
            do(counter_atom, []),
            forward(device0)
        )))
)
full_pipe = nic([(device0, c0_pipe)])
print(compile_dpdk(full_pipe))
})