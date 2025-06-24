"""Print backend IR to DPDK"""

import os, shutil, textwrap
from itertools import chain
from .backend import *

def indent(n, line):
    def nt(n): 
        return "\n" + " " * n
    return nt(n).join(line.split("\n"))

RX_BATCH_LEN = 32 # number of packets to pull from nic queue at a time

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

def pipe_return_vars(pipe : PipeBase) -> list[Var]:
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
            return_vars_by_case = []
            for k, v in cases.items():
                for return_var in pipe_return_vars(v):
                    return_vars_by_case.append(return_var)
            return return_vars_by_case
        case MainPipe(_, pipes):
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

def pipe_to_statement(pipe : PipeBase, toexit="return;"):
    """Print statements for a pipe. toexit is the string that prints 
       after an Exit(...), to terminate processing of current packet. 
       Use "return;" if you are in a segment that pulls from a queue, 
       one packet at a time, or "continue;" if you are in a segment 
       that pulls a batch from an RX device, and processes them 
       in a loop."""
    match pipe:
        case Atom(state, atom, args, return_var):
            # an "Atom" is a call to an atom function
            state_arg = default_state_arg if state is None else state
            args = (args + [return_var]) if return_var != None else args
            # state and packet args are globals
            argstrs = [state_arg.name, args[0].name]
            # pipe vars (including return vars) are all stored in the context
            for arg in args[1::]:
                if (type(arg)) in [Var, CompoundVar]:
                    argstrs.append(f"({arg.ty} *)&(ctx->{arg.namestr()})")
                elif (type(arg)) == CompoundVar:
                    argstrs.append(f"({arg.ty} *)&(ctx->{arg.namestr()})")
                elif (type(arg)) == Val:
                    argstrs.append(f"{arg.value}")
                elif (type(arg)) == StrLiteral:
                    argstrs.append(str(arg))
                
                else:
                    print(type(arg))
                    print(arg)
                    print("compiler error: invalid argument type in printer")
                    exit(1)

            return f"{atom.fn_name}({', '.join(argstrs)});"
        case MainPipe(_, _):
            raise Exception("MainPipe cannot be translated into a c statement")
        case Switch(var, cases):
            # a switch pipe translates into a switch statement
            case_strs = []
            for k, v in cases.items():
                if (k == None):
                    case_strs.append(f"\ndefault:{{\n{tablines(4, pipe_to_statement(v, toexit))  }\n}}")
                else:
                    inner = pipe_to_statement(v, toexit)+"\nbreak;"
                    case_strs.append(f"\ncase {k}:{{\n{tablines(4, inner)  }\n}}")
            return f"switch (ctx->{var.namestr()}) {{{tablines(4,''.join(case_strs))}\n}}"
        case Move(dst_queue):
            return f"if (0 != rte_ring_enqueue({dst_queue.name}, {mbuf.name})){{rte_pktmbuf_free(mbuf);}}\n{toexit}\n"
        case Seq(left, right):
            # a "Seq" is a sequence of two pipes
            return pipe_to_statement(left, toexit) + "\n" + pipe_to_statement(right, toexit)
        case Let(_, left, right):
            # local variables are already declared and the return is passed by ref, 
            # so this is just a sequence of two pipes
            return pipe_to_statement(left, toexit) + "\n" + pipe_to_statement(right, toexit)
        case At(_, inner_pipe):
            raise Exception("At should have been removed by now")
        case Exit(None):
            return f"rte_pktmbuf_free({mbuf.name});\n{toexit}"
        case Exit(dest):
            devnum = dest.devnum
            if (type(devnum)) == Var:
                # not an address because the tx function takes an integer
                devnum_str = f"ctx->{devnum.name}"
            else:
                devnum_str = str(devnum)
            stmts = [
                f"const uint16_t nb_tx = rte_eth_tx_burst({devnum_str}, {dest.queue}, &{mbuf.name}, 1);",
                f"if (nb_tx == 0) {{rte_pktmbuf_free({mbuf.name});}}\n{toexit}"]
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
    if (queue == None): # no queue -- pull from a NIC
        if_name, queue_num = segment.rx_dev.devnum, segment.rx_dev.queue   
        run_body = textwrap.dedent(f"""
            mario_ctx_t* ctx = rte_pktmbuf_mtod_offset({mbuf.name}, mario_ctx_t *, rte_pktmbuf_pkt_len({mbuf.name}));
            {packet_arg.ty} {packet_arg.name} = rte_pktmbuf_mtod({mbuf.name}, {packet_arg.ty});
            {indent(12, pipe_to_statement(segment.pipe, "continue;"))}
        """).strip()
        fcn=textwrap.dedent(f"""
            void {segment_function_name(segment)}(void) {{
                {mbuf.ty} {mbuf.name}s[{RX_BATCH_LEN}];
                {mbuf.ty} {mbuf.name} = NULL;
                const uint16_t nb_rx = rte_eth_rx_burst({if_name}, {queue_num}, {mbuf.name}s, {RX_BATCH_LEN});
                if (nb_rx > 0) {{
                    for (uint32_t i = 0; i < nb_rx; i++){{
                        mbuf = mbufs[i];
                        {indent(24, run_body)}
                    }}
                }}
                return;
            }}""").strip()
    else: # queue -- pull one packet at a time from it
        run_body = textwrap.dedent(f"""
            mario_ctx_t* ctx = rte_pktmbuf_mtod_offset({mbuf.name}, mario_ctx_t *, rte_pktmbuf_pkt_len({mbuf.name}));
            {packet_arg.ty} {packet_arg.name} = rte_pktmbuf_mtod({mbuf.name}, {packet_arg.ty});
            {indent(12, pipe_to_statement(segment.pipe))}
        """).strip()
        fcn=textwrap.dedent(f"""
            void {segment_function_name(segment)}(void) {{
                {mbuf.ty} {mbuf.name} = NULL;
                if (rte_ring_dequeue({queue}, (void **) &{mbuf.name}) == 0) {{
                    {indent(20, run_body)}
                }}
                return;
            }}""").strip()
    return fcn

def state_decl_init_of_pipe(pipe : PipeBase):
    match pipe:
        case Atom(state, atom, args, return_var):
            if (atom.state_ty == None):
                return []
            else:
             args = ", ".join([str(a) for a in atom.init_args])
             return [
                 (
                     f"{atom.state_ty} * {state.name};",
                    #  f"{atom.state_ty} {state.name}_v;\n{atom.state_ty} * {state.name} = &{state.name}_v;",
                     f"{state.name} = {atom.init_fn_name}({args});"
                 )
             ]
        case Seq(left, right):
            return state_decl_init_of_pipe(left) + state_decl_init_of_pipe(right)
        case Let(_, left, right):
            return state_decl_init_of_pipe(left) + state_decl_init_of_pipe(right)
        case At(_, inner_pipe):
            return state_decl_init_of_pipe(inner_pipe)
        case Switch(_, cases):
            return list(chain(*[state_decl_init_of_pipe(v) for k, v in cases.items()]))
        case MainPipe(_, pipes):
            return list(chain(*[state_decl_init_of_pipe(v) for k, v in pipes]))
        case Move(_):
            return []
        case Exit(_):
            return []
        case _:
            raise Exception("Unknown pipe type: "+str(type(pipe)))



def ty_defs_of_pipe(pipe : PipeBase):
    """Collect all the UserTy definitions from the pipe"""
    match pipe:
        case Atom(state, atomdecl, args, return_var):
            rv = []
            if (atomdecl.state_ty != None):
                s = atomdecl.state_ty.c_str()
                if (s != None):
                    rv.append(s)
            for aty in atomdecl.arg_tys:
                s = aty.c_str()
                if (s != None):
                    rv.append(s)
            if (atomdecl.ret_ty != None):
                s = atomdecl.ret_ty.c_str()
                if (s != None):
                    rv.append(s)
            return rv
        case Seq(left, right):
            return ty_defs_of_pipe(left) + ty_defs_of_pipe(right)
        case Let(_, left, right):
            return ty_defs_of_pipe(left) + ty_defs_of_pipe(right)
        case At(_, inner_pipe):
            return ty_defs_of_pipe(inner_pipe)
        case Switch(_, cases):
            return list(chain(*[ty_defs_of_pipe(v) for k, v in cases.items()]))
        case MainPipe(_, pipes):
            return list(chain(*[ty_defs_of_pipe(v) for k, v in pipes]))
        case Move(_):
            return []
        case Exit(_):
            return []
        case _:
            raise Exception("Unknown pipe type: "+str(type(pipe)))

def ty_defs_of_prog(irprog : IrProg):
    """get all of the user type c blocks from atoms in an IrProg"""
    defs = []
    for s in irprog.segments:
        if (s.pipe != None):
            defs += ty_defs_of_pipe(s.pipe)
    # delete duplicates
    seen = set()
    defs = [x for x in defs if not (x in seen or seen.add(x))]    
    return defs


def atom_defs_of_pipe(pipe : PipeBase):
    match pipe:
        case Atom(state, atomdecl, args, return_var):
            rv = []
            if (atomdecl.init != None): # there is optionally an init function
                # it must go first because it might also contain 
                #  type definitions used in the corresponding atom
                rv = rv + [atomdecl.init]
            rv = rv + [atomdecl.fn] # there is always a processing function
            return rv
        case Seq(left, right):
            return atom_defs_of_pipe(left) + atom_defs_of_pipe(right)
        case Let(_, left, right):
            return atom_defs_of_pipe(left) + atom_defs_of_pipe(right)
        case At(_, inner_pipe):
            return atom_defs_of_pipe(inner_pipe)
        case Switch(_, cases):
            return list(chain(*[atom_defs_of_pipe(v) for k, v in cases.items()]))
        case MainPipe(_, pipes):
            return list(chain(*[atom_defs_of_pipe(v) for k, v in pipes]))
        case Move(_):
            return []
        case Exit(_):
            return []
        case _:
            raise Exception("Unknown pipe type: "+str(type(pipe)))

def atom_defs_of_prog(irprog : IrProg):
    """get all of the extern c blocks from atoms in an IrProg"""
    defs = []
    for s in irprog.segments:
        if (s.pipe != None):
            defs += atom_defs_of_pipe(s.pipe)
    # delete duplicates
    seen = set()
    defs = [x for x in defs if not (x in seen or seen.add(x))]    
    return defs

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
    user_helpers = "\n".join([str(c) for c in irprog.cstr])
    user_tys = "\n".join(ty_defs_of_prog(irprog))
    state_init_str = state_init(irprog)
    atom_defs = "\n".join(atom_defs_of_prog(irprog))
    prog_ctx = prog_ctx_decl(irprog)
    queue_init_str = queue_inits(irprog)
    n_locs, loc_procs_str = loc_procs(irprog)
    segment_function_str = "\n".join([segment_function(s) for s in irprog.segments])
    return f"""
/******* compiler-generated includes ********/
{includes}
/********** user c code ***********/
{user_helpers}
/********** user types ***********/
{user_tys}
/********** atom definitions ***********/
{atom_defs}
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
# resources dir should be in parent of this script's dir
resources_dir = os.path.dirname(__file__) + "/../resources"

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
