#include <stdio.h>
#define PYTHON(x)

// user code
typedef struct {
    int ct[1024];
} counter_state_t;

typedef struct {
    int next_hop[1024];
} fwd_state_t;

typedef struct {
    int src;
    int dst;
    int ct;
} parsed_pkt_t;

typedef struct {
    int ct;
} counter_ret_t;

typedef struct {
    int next_hop;
} fwd_ret_t;

// atom functions

// parse atom: extract src, dst, and ct from the packet
PYTHON({parse_atom = atom(None, "parsed_pkt_t", "parse")})
void  parse(void* unused, char* pkt, parsed_pkt_t* rv) {
    rv->src = ((int*) pkt)[0];
    rv->dst = ((int*) pkt)[1];
    rv->ct = ((int*) pkt)[2];
    return;
}

// counter atom: increment the counter and return current value
PYTHON({counter_atom = atom("counter_state_t", "counter_ret_t", "counter")})
void counter(counter_state_t* state, char* pkt, parsed_pkt_t* p, counter_ret_t* rv) {
  state->ct[0] = state->ct[0] + 1;
  rv->ct = state->ct[0];
}

// fwd atom: select next hop for the packet from the array
PYTHON({fwd_atom = atom("fwd_state_t", "fwd_ret_t", "fwd")})
void fwd(fwd_state_t* state, char* pkt, parsed_pkt_t* p, counter_ret_t* ct, fwd_ret_t* rv) {
  printf("fwd: ct=%d\n", ct->ct);
  rv->next_hop = state->next_hop[0];  
  // test: set the first word of the packet to 0
  ((int*)pkt)[0] = 0;
  return;
}

PYTHON({
p1 =let("parsed_pkt", do(parse_atom, []),
    let("ct", do(counter_atom, ["parsed_pkt"]),
    let("ct", do(counter_atom, ["parsed_pkt"]),    
    do(fwd_atom, ["parsed_pkt", "ct"]
))));
print(compile("p1", p1))
})


int main() {
    char pkt[] = "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\x0f";
    p1(pkt);
    printf ("--- final packet ---\n");
    for (int i = 0; i < sizeof(pkt); i++) {
      printf("%02x ", (unsigned char)pkt[i]);
    }
    printf("\n");
}