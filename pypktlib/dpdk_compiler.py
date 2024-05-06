"""
    Compile a pipeline to a C-DPDK function. 
    Current step: add locations and location analysis, working towards the IR.

"""
from syntax import *

default_state_arg = "_no_state"
packet_arg = "pkt"

global idctr; idctr = -1
def fresh_name(name):
    global idctr
    idctr = idctr + 1
    return f"{name}_{idctr}"

#### frontend passes
def instantiate(pipe : PipeBase):
    """
    instantiate a unique instance of each atom in the pipe, 
    by giving each atom a unique state variable name.
    """
    match pipe: 
        case Atom(None, atom, args, return_var):
            if (has_state(atom)):
                return replace(pipe, state=fresh_name(name(atom)+"_state"))
            else:
                return pipe
        case Seq(left, right):
            return replace(pipe, left=instantiate(left), right=instantiate(right))
        case Let(_, left, right):
            return replace(pipe, left=instantiate(left), right=instantiate(right))
        case At(_, inner_pipe):
            return replace(pipe, inner_pipe=instantiate(inner_pipe))
        case Exit(_):
            return pipe
            
def rename_vars(renames : dict[str, str], pipe : PipeBase):
    """rename variables in let bindings to prevent shadowing"""
    match pipe:
        case Atom(_, _, args, _):
            # for an Atom, rename the arguments to the updated names
            renamed_args = []
            for arg in args:
                if arg in renames:
                    renamed_args.append(renames[arg])
                else:
                    raise Exception(f"Unbound variable {arg}")
            return replace(pipe, args=renamed_args)
        case Seq(left, right):
            # sequences don't bind or return variables.
            return replace(pipe, left=rename_vars(renames, left), right=rename_vars(renames, right))
        case Let(return_name, left, right):
            # for a Let, rename the return variable and replace all occurences in the next pipe
            fresh_return_name = fresh_name(return_name)
            inner_renames = {r: f for r, f in renames.items() if r != return_name}
            inner_renames = {**inner_renames, **{return_name: fresh_return_name}} # add the fresh return name
            return replace (pipe, return_name=fresh_return_name, left=rename_vars(renames, left), right=rename_vars(inner_renames, right))
        case At(location, inner_pipe):
            return replace(pipe, inner_pipe=rename_vars(renames, inner_pipe))
        case Exit(_):
            return pipe

def assign_return_vars(return_var : str, pipe : PipeBase):
    """set each atom's return variable. Must be run _after_ rename_vars. """
    match pipe:
        # case: an atom with no return variable, and the current pipe does not have a name for its return variable
        # (we need to name the return variable to generate the c code, so just generate a fresh name)
        case Atom(_, atom, _, None):
            if (return_var == None):
                return_var = fresh_name("_unused"+name(atom)+"_ret")
            return replace (pipe, return_var=return_var)
        # case: sequence of two pipes. Only the return of the second pipe is used.
        case Seq(left, right):
            return replace(pipe, left=assign_return_vars(None, left), right=assign_return_vars(return_var, right))
        # case: let binding. The return variable of the left pipe is bound to the name in the let binding.
        case Let(return_name, left, right):
            return replace(pipe, left=assign_return_vars(return_name, left), right=assign_return_vars(return_var, right))
        # case: change the pipe's location -- this has no effect on the return variables
        case At(_, inner_pipe):
            return replace(pipe, inner_pipe=assign_return_vars(return_var, inner_pipe))
        case Exit(_):
            return pipe



# location analysis
def locate_pipes(pipe : PipeBase, cur_loc : str):
    match pipe:
        case Atom(_, _, _, _):
            print(f"Setting start and end to {cur_loc} in pipe {pipe}")
            return replace (pipe, start=cur_loc, end=cur_loc)
        case Seq(left, right):
            # a sequence starts at the start of the left pipe, and ends at the end of the right pipe
            # the left pipe's end is the right pipe's start
            new_left = locate_pipes(left, cur_loc)
            new_right = locate_pipes(right, new_left.end)
            seq_start = new_left.start
            seq_end = new_right.end
            return replace(pipe, left=new_left, right=new_right, start=seq_start, end=seq_end)
        case Let(_, left, right):
            # a let pipe has the same location rules as a sequence
            new_left = locate_pipes(left, cur_loc)
            new_right = locate_pipes(right, new_left.end)
            seq_start = new_left.start
            seq_end = new_right.end
            return replace(pipe, left=new_left, right=new_right, start=seq_start, end=seq_end)
        case At(location, inner_pipe):
            # an at pipe changes the location of the inner pipe
            # its own location starts at the current location, and ends at the specified location
            new_inner = locate_pipes(inner_pipe, location)
            return replace(pipe, inner_pipe=new_inner, start=cur_loc, end=location)
        case Exit(_):
            # TODO: design choice: should an exit change the location of the pipe?
            return replace(pipe, start=cur_loc, end=cur_loc)


#### Toplevel compiler function
def compile(hdl_name : str, pipe : PipeBase):
    print(f"Compiling {hdl_name}")
    print("--- frontend passes ---")
    # 1. instantiate the pipe (name for state variables)
    ip = instantiate(pipe)
    # 2. names for variables
    ip = rename_vars({}, ip)
    # 3. bind return variables to atoms
    ip = assign_return_vars(None, ip)
    # 4. location analysis
    ip = locate_pipes(ip, "start")
    # 4. generate the code blocks
    print("--- frontend complete ---")
    return(pretty_print_located(ip))


def test():
    parse_atom = atom(None, "parsed_pkt_t", "parse", [])
    counter_atom = atom("counter_state_t", "counter_ret_t", "counter", [])
    fwd_atom = atom("fwd_state_t", "fwd_ret_t", "fwd", ["parsed_pkt_t", "counter_ret_t"])
    p1 =at("c0", 
            let("parsed_pkt", do(parse_atom, []),
            at("c1", 
                let("ct", do(counter_atom, ["parsed_pkt"]),
                seq(
                do(fwd_atom, ["parsed_pkt", "ct"]),
                fwd("eth0")
    )))));
    print("--- initial program--- ")
    print(pipe_to_string(p1))
    ret_str = compile("p1", p1)
    print("Generated code:")
    print(ret_str)


if __name__ == '__main__':
    test()
