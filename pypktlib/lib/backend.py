"""
    Compile a pipeline to a C-DPDK function. 
"""
import os, shutil
from itertools import chain
from .syntax import *
#### Pipeline IR

# the IR is a graph of named, located pipe segments.
# pipes have a new constructor, Move, that represents the effectful move from one segment to another.

@dataclass(frozen=True)
class Segment():
    """ A Named pipe segment """
    name      : str = None
    rx_queue  : str = None
    rx_dev    : tuple[str, int] = None # if you enter the segment from the network, this will be filled
    location  : str = None
    pipe      : PipeBase = None
    __match_args__ = ('name', 'rx_queue', 'location', 'pipe')
    def __str__(self):
        return f"segment {self.name} at {self.location}:{newtab(2)}{tablines(2, pretty_print(self.pipe))}"

@dataclass(frozen=True)
class Queue():
    """ A typed queue """
    name      : str = None
    src_seg   : str = None
    dst_seg   : str = None
    msg_ty    : list[Var] = None
    def __str__(self):
        return f"queue {self.name} ({self.src_seg} -> {self.dst_seg}) : {self.msg_ty}"
    

@dataclass(frozen=True)
class Move(PipeBase):
    """
        Move(dst_queue) is a new pipe in the IR that represents 
        an effectful move from one segment / location to another.
    """
    dst_queue: Queue = None
    __match_args__ = ('dst_queue',)
    def __str__(self):
        if self.dst_queue == None:
            return f"move<to software>)"
        return f"move<{self.dst_queue.msg_ty}>({self.dst_queue.name})"


@dataclass(frozen=True)
class IrProg():
    """ A program in the IR """
    segments : list[Segment] = None
    queues   : list[Queue] = None
    def __str__(self):
        lines = []
        lines.append("segments:")
        for s in self.segments:
            lines.append(str(s))
        lines.append("queues:")
        for q in self.queues:
            lines.append(str(q))
        return "\n".join(lines)        

def fresh_segment_name(loc): 
    return fresh_name(str(loc)+"_segment")

def fresh_queue_name():
    return fresh_name("queue")

def pipe_to_segments(prog: IrProg, cur_seg_name : str, bound_vars : list[Var], pipe : PipeBase, rx_dev=None):
    """
    Convert a pipe program into a graph of segments connected by queues.
    """
    match pipe:
        case At(location, inner_pipe):
            # an "at" pipe starts a new segment.
            next_name = fresh_segment_name(location)
            if (rx_dev == None):
                # build the queue that connects the two segments
                next_queue = Queue(name=fresh_queue_name(), src_seg=cur_seg_name, dst_seg=next_name, msg_ty=bound_vars)
                # build the next segment
                prog, after_next_name, next_pipe = pipe_to_segments(prog, next_name, bound_vars, inner_pipe)
                next_segment = Segment(name=next_name, rx_queue=next_queue.name, location=location, pipe=next_pipe)
                # finally, return the move pipe, which is just an instruction to put the current environment
                # into a message for the queue and then return.
                rv = Move(dst_queue=next_queue)
                rv = replace(rv, start=pipe.start, end=pipe.end)
                return IrProg(prog.segments+[next_segment], prog.queues+[next_queue]), after_next_name, rv
            else:
                # build the next segment
                prog, after_next_name, next_pipe = pipe_to_segments(prog, next_name, bound_vars, inner_pipe)
                next_segment = Segment(name=next_name, rx_queue=None, location=location, pipe=next_pipe)
                next_segment = replace(next_segment, rx_dev=rx_dev)
                rv = Move(dst_queue=None)
                return IrProg(prog.segments+[next_segment], prog.queues), after_next_name, rv
        case Seq(left, right):
            prog, after_left_name, new_left = pipe_to_segments(prog, cur_seg_name, bound_vars, left)
            if (after_left_name != cur_seg_name):
                print("unexpected: sequence with a move in the left side")
                exit(1)
            prog, after_right_name, new_right = pipe_to_segments(prog, after_left_name, bound_vars, right)
            return prog, after_right_name, replace(pipe, left=new_left, right=new_right)
        case Let(ret, left, right):
            prog, after_left_name, new_left = pipe_to_segments(prog, cur_seg_name, bound_vars, left)
            if (after_left_name != cur_seg_name):
                print("unexpected: let with a move in the left side")
                exit(1)
            new_bound_vars = bound_vars + [ret]
            prog, after_right_name, new_right = pipe_to_segments(prog, after_left_name, new_bound_vars, right)
            return prog, after_right_name, replace(pipe, left=new_left, right=new_right)
        case Atom(_, _, _, _):
            # atoms don't change the segment or bind variables. No change. 
            return prog, cur_seg_name, pipe    
        case Exit(_):
            # exit pipes don't change the segment or bind variables. No change.
            return prog, cur_seg_name, pipe
        case Switch(_, cases):
            # switch pipes don't change the segment or bind variables.
            # Also, the switch is inlined, so we don't need to know the names of the segments that 
            # it jumps to, or the variables that are bound in each case. 
            # but, we do have to recurse on the cases, to segment those pipes.
            new_cases = {}
            for k, v in cases.items():
                prog, _, new_case = pipe_to_segments(prog, cur_seg_name, bound_vars, v)
                new_cases[k] = new_case
            return prog, cur_seg_name, replace(pipe, cases=new_cases)
        case NicDeliver(_, pipes):
            new_pipes = []
            for k, v in pipes:
                prog, _, new_pipe = pipe_to_segments(prog, cur_seg_name, bound_vars, v, rx_dev=k)
                new_pipes.append((k, new_pipe))
            return prog, cur_seg_name, replace(pipe, pipes=new_pipes)
        
            
def pipe_to_ir(pipe : PipeBase):
    # the main pipe is a special case, because it doesn't have a name or location
    # and it doesn't have a return variable. 
    # we'll give it a fresh name and location, and a return variable, 
    # and then run the normal pipe_to_segments on it.
    main_segment_name = fresh_segment_name(start_loc)
    # note: the packet argument is a global variable, hence why we pass it to bound vars
    prog, _, main_segment_pipe = pipe_to_segments(IrProg([], []), main_segment_name, [packet_arg], pipe)
    main_segment = Segment(name=main_segment_name, location=start_loc, pipe=main_segment_pipe)
    segments = prog.segments + [main_segment]
    segments = segments[::-1]
    prog = IrProg(segments, prog.queues)
    return prog


def backend_passes(pipe : PipeBase):
    # 6. convert to segment-graph IR
    return pipe_to_ir(pipe)


#### DPDK code generation


mbuf = Var("mbuf", "struct rte_mbuf *")

def queue_decl(queue : Queue):
    return f"struct rte_ring* {queue.name} = NULL;"
def queue_init(queue: Queue):
    return f"{queue.name} = rte_ring_create(\"{queue.name}\", RING_SIZE, rte_socket_id(), RING_F_SP_ENQ | RING_F_SC_DEQ);"

queue_init_fname = "init_queues"
def queue_inits(irprog : IrProg):
    decl_str = "\n".join([queue_decl(q) for q in irprog.queues])
    init_str = "\n".join([queue_init(q) for q in irprog.queues])
    init_fcn = f"""
void {queue_init_fname}(void) {{
    {init_str}
}}
"""
    ret = "\n" + decl_str + "\n" + init_fcn
    return ret

def pipe_return_vars(pipe : PipeBase):
    """
        Get the return variables of all the atoms in the pipe
    """
    match pipe:
        case Atom(_, _, _, return_var):
            # atoms might not return anything
            if (return_var != None):
                return [return_var]
            else:
                return []
        case Seq(left, right):
            return pipe_return_vars(left) + pipe_return_vars(right)
        case Let(_, left, right):
            return pipe_return_vars(left) + pipe_return_vars(right)
        case At(_, inner_pipe):
            return pipe_return_vars(inner_pipe)
        case Switch(_, cases):
            lists = [v for k, v in cases.items() for v in pipe_return_vars(v)] 
            return list(chain(*lists))
        case NicDeliver(_, pipes):
            lists = [pipe_return_vars(v) for k, v in pipes]
            return list(chain(*lists))
        case Move(_):
            return []
        case Exit(_):
            return []

def prog_ctx_decl(irprog : IrProg):
    rvs = []
    for s in irprog.segments:
        rvs = rvs + (pipe_return_vars(s.pipe))
    rvs = list(set(rvs))
    fields_str = tablines(4, "\n".join([f"{v.ty} {v.name};" for v in rvs]))
    return f"""
typedef struct mario_ctx_t {{
{fields_str}
}} mario_ctx_t;
"""

def pipe_to_statement(pipe : PipeBase):
    match pipe:
        case Atom(state, atom, args, return_var):
            # an "Atom" is a call to an atom function
            state_arg = default_state_arg if state is None else state
            args = (args + [return_var]) if return_var != None else args
            # state and packet args are globals
            argstrs = [state_arg.name, args[0].name]
            # pipe vars (including return vars) are all stored in the context
            for arg in args[1::]:
                argstrs.append(f"({arg.ty} *)&(ctx->{arg.name})")
            return f"{atom.fn_name}({', '.join(argstrs)});"
        case NicDeliver(_, _):
            raise Exception("NicDeliver cannot be translated into a c statement")
        case Switch(var, cases):
            # a switch pipe translates into a switch statement
            case_strs = []
            for k, v in cases.items():
                case_strs.append(f"case {k}: {{ {pipe_to_statement(v)} }} ")
            return f"switch ({var}) {{ {' '.join(case_strs)} }}"
        case Move(dst_queue):
            return f"rte_ring_enqueue({dst_queue.name}, {mbuf.name});"
        case Seq(left, right):
            # a "Seq" is a sequence of two pipes
            return pipe_to_statement(left) + "\n" + pipe_to_statement(right)
        case Let(_, left, right):
            # local variables are already declared and the return is passed by ref, 
            # so this is just a sequence of two pipes
            return pipe_to_statement(left) + "\n" + pipe_to_statement(right)
        case At(_, inner_pipe):
            raise Exception("At should have been removed by now")
        case Exit(None):
            return f"rte_pktmbuf_free({mbuf.name});\nreturn;"
        case Exit(dest):
            stmts = [
                f"const uint16_t nb_tx = rte_eth_tx_burst({dest.devnum}, {dest.queue}, &{mbuf.name}, 1);",
                f"if (nb_tx == 0) {{rte_pktmbuf_free({mbuf.name});}}",
                "return;"]
            return "\n".join(stmts)            
        case _:
            raise Exception("Unknown pipe type: "+str(type(pipe)))

def segment_function_name(segment : Segment):
    return f"run_segment_{segment.name}"

def segment_function(segment : Segment):
    # segments that run in the network don't get printed to c
    if (segment.location == start_loc):
        return ""
    queue = segment.rx_queue
    # if queue is none, this is a segment reading from the network and rx_dev should be set
    run_body = f"""
    mario_ctx_t* ctx = rte_pktmbuf_mtod_offset({mbuf.name}, mario_ctx_t *, rte_pktmbuf_pkt_len({mbuf.name}));
    {packet_arg.ty} {packet_arg.name} = rte_pktmbuf_mtod({mbuf.name}, {packet_arg.ty});
    {tablines(8, pipe_to_statement(segment.pipe))}
    """
    if (queue == None):
        if_name, queue_num = segment.rx_dev.devnum, segment.rx_dev.queue
        fcn=f"""
void {segment_function_name(segment)}(void) {{
    {mbuf.ty} {mbuf.name} = NULL;
    const uint16_t nb_rx = rte_eth_rx_burst({if_name}, {queue_num}, &{mbuf.name}, 1);
    if (nb_rx > 0) {{{run_body}}}
    return;    
}}"""
    else:
        fcn=f"""
void {segment_function_name(segment)}(void) {{
    {mbuf.ty} {mbuf.name} = NULL;
    if (rte_ring_dequeue({queue}, (void **) &{mbuf.name}) == 0) {{{run_body}}}
    return;
}}"""
    return fcn
 
def state_decl_init_of_pipe(pipe : PipeBase):
    match pipe:
        case Atom(state, atom, args, return_var):
            if (atom.state_ty == None):
                return []
            else:
             return [
                 (f"{atom.state_ty} {state.name}_v;\n{atom.state_ty} * {state.name} = &{state.name}_v;",
                 f"{state.name} = {atom.init_fn_name}();")
             ]
        case Seq(left, right):
            return state_decl_init_of_pipe(left) + state_decl_init_of_pipe(right)
        case Let(_, left, right):
            return state_decl_init_of_pipe(left) + state_decl_init_of_pipe(right)
        case At(_, inner_pipe):
            return state_decl_init_of_pipe(inner_pipe)
        case Switch(_, cases):
            return list(chain(*[state_decl_init_of_pipe(v) for k, v in cases.items()]))
        case NicDeliver(_, pipes):
            return list(chain(*[state_decl_init_of_pipe(v) for k, v in pipes]))
        case Move(_):
            return []
        case Exit(_):
            return []
        case _:
            raise Exception("Unknown pipe type: "+str(type(pipe)))


state_init_fname = "init_state"
def state_init(irprog : IrProg):
    decls_inits = [(f"void * {default_state_arg.name} = NULL;", "")]
    for s in irprog.segments:
        decls_inits = decls_inits + state_decl_init_of_pipe(s.pipe)
    decls_inits = list(set(decls_inits))
    decls = [d[0] for d in decls_inits]
    inits = [d[1] for d in decls_inits]
    inits_str = tablines(4, "\n".join(inits))
    init_function = f"""
void {state_init_fname}(void) {{
    {inits_str}
}}
"""
    return "\n"+"\n".join(decls) + "\n" + init_function

def loc_proc(segments : list[Segment], loc_name : str):
    """infinite loop function that runs all the segments at a location"""
    fname = f"core_{loc_name}"
    segment_fcn_calls = [f"{segment_function_name(s)}();" for s in segments]
    segment_fcns_str = tablines(4, "\n".join(segment_fcn_calls))
    fcn = f"""
static int {fname}(void *arg __attribute__((unused))) {{
    printf("Core %u running location {loc_name}\\n", rte_lcore_id());
    while(1) {{
        {segment_fcns_str}
    }}
    return 0;
}}
"""
    return (fname, fcn)

loc_proc_listname = "location_processes"
def loc_procs(irprog : IrProg):
    """ Get a list of names and processes for each location in the ir program """
    loc_dict = {} # location -> segments
    for s in irprog.segments:
        if (s.location != start_loc):
            if (s.location not in loc_dict):
                loc_dict[s.location] = []
            loc_dict[s.location].append(s)
    loc_proc_names = []
    loc_procs = []
    for loc, segments in loc_dict.items():
        if (loc != start_loc):
            name, fcn = loc_proc(segments, loc)
            loc_proc_names.append(name)
            loc_procs.append(fcn)
    loc_proc_str = "\n".join(loc_procs)
    loc_proc_name_str = ",\n".join(loc_proc_names)
    loc_proc_ptr_list = f"static int (*{loc_proc_listname}[])(void *) = {{\n{loc_proc_name_str}\n}};"
    return len(loc_dict), loc_proc_str + "\n" + loc_proc_ptr_list


def main_fcn(n_loc_procs : int):
    main = f"""
int main(int argc, char *argv[])
{{	
    dpdk_init(cfg, argc, argv); // init dpdk, ports, and memory pools
    printf("DPDK initialized\\n");
    {state_init_fname}(); // init atom states
    {queue_init_fname}(); // init queues
    // get all the lcores
    int core_count = rte_lcore_count();
    // one extra core for main
    if (core_count < {n_loc_procs}+1) {{
        printf("Error: There are {n_loc_procs} location processes, but only %d cores.\\n", core_count);
        rte_eal_cleanup();
        return -1;
    }}
    uint32_t proc_idx = 0;
    uint32_t lcore_id;
    RTE_LCORE_FOREACH_WORKER(lcore_id) {{
        if (proc_idx < {n_loc_procs}) {{
            rte_eal_remote_launch(location_processes[proc_idx], NULL, lcore_id);
            proc_idx++;
        }}
    }}
    printf("\\nMain Core %u waiting on workers. [Ctrl+C to quit]\\n", rte_lcore_id());
    while(1) {{
        sleep(1);
    }}
    rte_eal_cleanup();
    return 0;
}}"""
    return main

includes = """
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
"""

config = """
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
"""

def irprog_to_dpdkcode(irprog : IrProg):
    prog_ctx = prog_ctx_decl(irprog)
    state_init_str = state_init(irprog)
    queue_init_str = queue_inits(irprog)
    n_locs, loc_procs_str = loc_procs(irprog)
    segment_function_str = "\n".join([segment_function(s) for s in irprog.segments])
    return f"""
/******* compiler-generated code ********/
{includes}
/********** pipeline context / metadata ***********/
{prog_ctx}
/********** config ***********/
{config}
/********** queue declarations ***********/
{queue_init_str}
/********** atom state  ***********/
{state_init_str}
/********** segment functions ***********/
{segment_function_str}
/********** location processes ***********/
{loc_procs_str}
/********** main ***********/
{main_fcn(n_locs)}
"""

# DPDK program build directory setup
resources_dir = "./resources"
resources = [
    "Makefile", 
    "dpdk_init.h",
    "pcaprun.sh"
]

def copy_dpdk_resources(base_dir : str):
    for r in resources:
        src = f"{resources_dir}/{r}"
        tgt = f"{base_dir}/{r}"
        shutil.copy(src, tgt)
