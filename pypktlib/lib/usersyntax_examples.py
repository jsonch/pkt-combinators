from usersyntax import *

### Examples
def examples():
    # define some abstract types
    uint = Ty.uint      # Ty is a class that dynamically generates                      
    pkt_t = Ty.pkt_t    # attributes with values equal to the string of their names
    tbl  = Ty.tbl
    pkt  = Ty.pkt
    ip_hdr_t = Ty.ip_hdr_t

    ## Defining Atoms
    # A stateless atom has a type signature, a function name, 
    # and a function definition
    # TODO: make atoms take init args as a second args tuple, so you can 
    # have a parametric library of atoms.
    parse = Atom[None, (ref[pkt_t]), ref[ip_hdr_t]](
        name="parse_eth_ip",
        f = """
            void parse_eth_ip(pkt_t* pkt, ip_hdr_t* hdr){
                if (pkt_len > 30){
                    hdr->src = pkt[26];
                    hdr->dst = pkt[30];
                }
            }
        """
    )

    # A stateful atom also has an initializer and initializer arguments
    # the state is the second arg in the C function
    lookup_egr = Atom[Ptr[Ty.tbl], (Ptr[ip_hdr_t]), Ptr[Ty.int]](
        name="lookup",
        f="""
            void lookup(pkt* p, tbl* t, ip_hdr_t* ip_hdr, int* outval) {
                *outval = tbl_lookup(t, ip_hdr->dst);
                return;
            }
        """,
        init = """
            tbl* mkTbl(int len) {
                return malloc(len * 4);
            }
        """,
        init_args = [1024]
    )


    count_flow = Atom[Ptr[Ty.tbl], ref[ip_hdr_t], None](
        name="count",
        f = "void count(pkt* p, tbl* t, ip_hdr_t* hdr){ //TODO }",
        init="tbl* mkTbl(int len) { return malloc(len * 4); }",
        init_args=[1024]
    )

    # builtin atoms
    hash = Atom[None, None, ref[Ty.uint]]("calchash", "//todo")
    rand = Atom[None, None, ref[Ty.uint]]("calcrand", "//todo")

    ## Making pipes
    #  Pipes can be constructed by: 
    #  1. calling an atom
    #  2. sequencing two pipes together
    #  3. binding the output of a pipe to a variable and calling a second pipe

    # parse the ip header, lookup the egress port, count the flow, and send the packet
    parse_lookup_count_send = Pipe(
        Meta.ip_hdr <- parse() % # let ip_hdr = parse() in  -- the % is the "in" token
        Meta.egr_port <- lookup_egr(Meta.ip_hdr) %
        (count_flow(Meta.ip_hdr) >> Send(Meta.egr_port)) # ">>" sequences two pipes together
    )
    # Notes: 
    # 1. The Meta object is a namespace for variables
    # 2. The "packet" argument is implicit in all atom calls.
    # 3. The sequence operator passes only the packet from pipe to pipe, 
    #    discarding all other outputs.

    print("----parse_lookup_count_send----")
    print(parse_lookup_count_send)

    ## Making pipes pt. 2
    # Pipes can also be constructed by:
    # 4. branching / switching on a value to select the next pipe

    # parse the ip header, lookup the egress port, and either drop or send the packet
    parse_lookup_drop_or_send = Pipe(
        Meta.ip_hdr <- parse() %
        Meta.egr_port <- lookup_egr(Meta.ip_hdr) %
        Switch(Meta.egr_port)({
            0:Stop(),
            default:Send(Meta.egr_port)
        })
    )
    print("----parse_lookup_drop_or_send----")
    print(parse_lookup_drop_or_send)

    ## Pipe locations
    # You can locate a pipe's compute by pinning 
    # it to a core. 

    # hash to pick the core
    shard_4 = Pipe(
        Meta.selected_core <- hash(4) %
        Switch(Meta.selected_core)({
            0:parse_lookup_count_send @ Core[0], # At(Core[0], parse_lookup_...)
            1:parse_lookup_count_send @ Core[1],
            2:parse_lookup_count_send @ Core[2],
            default:parse_lookup_count_send @ Core[3]
        })
    )
    print("----shard_4----")
    print(shard_4)

    ## Atom state sharing
    # By default, every Atom pipe has its own instance of state. 
    # So count() >> count() will apply count twice, each with 
    # a different state data structure. 
    # You can use "[]" in an Atom call to use a named state instance, so
    # count[State.ct1]() >> count[State.ct2] will apply count twice, 
    # using the same data structure in both calls. 
    # Once you bind a pipe to a name with "Pipe(...)",
    # you can make the atoms in the pipe use new, unshared 
    # states by using the "New" combinator.
    parallel_4 = Pipe(
        Meta.selected_core <- rand(4) %
        Switch(Meta.selected_core)({
            0:New(parse_lookup_count_send) @ Core[0],
            1:New(parse_lookup_count_send) @ Core[1],
            2:New(parse_lookup_count_send) @ Core[2],
            default:New(parse_lookup_count_send) @ Core[3]
        })
    )
    print("----parallel_4----")
    print(parallel_4)


    ## Making pipes pt. 3
    # Since pipes are just python expressions / objects, 
    # they can be constructed by your own custom combinators, 
    # which are just python functions that return / transform pipes!
    def Parallel(n_cores, pipe):
        """Load balance pipe across n_cores via packet 
           spraying. Use separate state for each pipe."""        
        cases = {i:New(pipe) @ Core[i] for i in range(n_cores-1)}
        cases[default] = New(pipe) @ Core[n_cores-1]
        return Pipe(
            Meta.selected_core <- rand(n_cores) %
            Switch(Meta.selected_core)(cases))

    parallel_pipe = Parallel(4, parse_lookup_count_send)

    print("----parallel_pipe----")
    print(parallel_pipe)

    def Shard(n_cores, pipe):
        """Load balance across n_cores by sharding. 
           Use shared state for all pipes."""
        cases = {i:pipe @ Core[i] for i in range(n_cores-1)}
        cases[default] = pipe @ Core[n_cores-1]
        return Pipe(
            Meta.selected_core <- hash(n_cores) %
            Switch(Meta.selected_core)(cases))
    sharded_pipe = Shard(4, parse_lookup_count_send)
    print("----sharded_pipe----")
    print(sharded_pipe)


    ## Inputs and outputs
    # there are three special atoms for IO:
    # Receive(port_id): fetch a packet from the given port
    # Send(port_id): send the current packet to the specified port
    # Stop(): stop processing the packet
    # Send and stop are separated to allow for 
    # multicast / packet copying. 
    # Receive is the only way to introduce a packet into the system, 
    # and is only allowed to come at the beginning of a pipe.
    # Stop is implicitly added to the end of every pipe.

    parse_lookup_count_send_full = Pipe(
        Receive({"dev":0, "queue":0}) >>        
        Meta.ip_hdr <- parse() %
        Meta.egr_port <- lookup_egr(Meta.ip_hdr) %
        (count_flow(Meta.ip_hdr) >> Send(Meta.egr_port)
        >> Stop() # optional                 
        )
    )
    print("----parse_lookup_count_send_full----")
    print(parse_lookup_count_send_full)

    ## Programs
    # A program is a list of pipes, where each 
    # input dev is used in a Receive at most once.
    prog = [
       Pipe(Receive(0) >> (parse() >> lookup_egr() >> Send(Meta.port)) @ Core[0]), 
       Pipe(Receive(1) >> (parse() >> lookup_egr() >> Send(Meta.port)) @ Core[0]), 
    ]



def main():
  print ("frontend syntax examples")
  examples()

if __name__ == '__main__':
  main()