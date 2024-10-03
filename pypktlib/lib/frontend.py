"""frontend passes"""
from .syntax import *
import copy

def recurse(f, pipe):
    """helper to recurse on cases that do nothing"""
    match pipe:
        case Atom(_, _, _, _):
            return pipe
        case Seq(left, right):
            return replace(pipe, left=f(left), right=f(right))
        case Let(var, left, right):
            return replace(pipe, left=f(left), right=f(right))
        case At(_, inner_pipe):
            return replace(pipe, inner_pipe=f(inner_pipe))
        case Exit(_):
            return pipe
        case Switch(_, cases):
            new_cases = {k: f(v) for k, v in cases.items()}
            return replace(pipe, cases=new_cases)
        case MainPipe(_, pipes): 
            new_pipes = [(k, f(v)) for k, v in pipes]
            return replace(pipe, pipes=new_pipes)
        case _: 
            print(type(pipe))
            print("ERROR IN RECURSE!")
            exit(1)


def instantiate(pipe : PipeBase):
    """
    instantiate a unique instance of each atom in the pipe, 
    by giving each atom a unique state variable name.
    """
    match pipe: 
        case Atom(None, atom, args, return_var):
            if (atom.state_ty != None):
                state_var = Var(name=fresh_name(name(atom)+"_state"), ty=atom.state_ty)
                return replace(pipe, state=state_var)
            else:
                return pipe
        case _: return recurse(instantiate, pipe)

def rename_vars(renames : dict[str, str], pipe : PipeBase):
    """rename variables in let bindings to prevent shadowing"""
    def recurse_with_renames(p):
        rv = rename_vars(renames, p)
        return rv
    match pipe:
        case Atom(_, _, args, _):
            # for an Atom, rename the arguments to the updated names
            renamed_args = []
            for arg in args:
                if type(arg) in [Val, StrLiteral]:
                    renamed_args.append(arg) # do nothing
                else:
                    # note: we only ever have to rename base names
                    if arg.base_name() in renames:
                        new_name =renames[arg.base_name()]
                        new_arg = arg.base_rename(new_name)
                        renamed_args.append(new_arg)
                    else:
                        print("arg.name: >>", arg.name,"<<")
                        print("-----renames-----")
                        for k, v in renames.items():
                            print(f"{k} -> {v}")
                            print(type(k))
                            print(type(arg.name))
                            print("looking for:", arg.name)
                            if k == arg.name:
                                print("found? why are we here?")
                        raise Exception(f"Unbound variable {str(arg.name)}")
            rv = replace(pipe, args=renamed_args)
            return rv
        case Exit(dest):
            rename_str_keys = {str(k): v for k, v in renames.items()}
            if dest.devnum != None and str(dest.devnum) in rename_str_keys:
                new_dest = replace(dest, devnum=rename_str_keys[str(dest.devnum)])
                return replace(pipe, dest=new_dest)
            else:
                return pipe
        case Let(ret, left, right):
            # for a Let, rename the return variable and replace all occurences in the next pipe            
            fresh_ret = fresh_var(ret)
            inner_renames = {r: f for r, f in renames.items() if r != ret.name}
            inner_renames = {**inner_renames, **{ret.name: fresh_ret.name}} # add the fresh return name
            # print("let pipe: ", pretty_print(pipe))
            # print("---inner_renames---")
            # for k, v in inner_renames.items():
            #     print(f"{k} -> {v}")
            rv = replace (pipe, ret=fresh_ret, left=rename_vars(renames, left), right=rename_vars(inner_renames, right))
            return rv
        case Switch(var, cases):
            if var.name in renames:
                new_var = renames[var.name]
            else:
                raise Exception(f"Unbound variable in switch expression {str(var.name)}")
            new_cases = {}
            for (k, inner_pipe) in cases.items():
                new_pipe = rename_vars(renames, inner_pipe)
                new_cases[k] = new_pipe
            rv = replace(pipe, var=new_var, cases=new_cases)
            return rv
        case _: 
            # for all other cases, recurse with the same renames
            rv = recurse(recurse_with_renames, pipe)
            return rv

def type_pipeline_vars(cur_ret : Var, pipe : PipeBase):
    """Set each atom's return variable and infer types for all pipe variables. 
       Must be run _after_ rename_vars. """
    match pipe:
        # case: an atom with no return variable, and the current pipe does not have a name for its return variable
        # (we need to name the return variable to generate the c code, so just generate a fresh name)
        case Atom(_, atom, args, None):
            new_args = []
            for arg, ty in zip(args, atom.arg_tys):
                new_args.append(argty(arg, ty))
            if (atom.ret_ty == None):
                if (cur_ret != None):
                    print(f"Error: atom {atom.name} has no return type, but is used in a pipe that expects a return value from it.")
                    exit(1)
                else:
                    return None, replace (pipe, args=new_args)
            else: # the atom has a return type, so we will create a return variable no matter what.
                if (cur_ret == None):
                    cur_ret = Var(name=fresh_name("_unused"+name(atom)+"_ret"), ty=atom.ret_ty)
                else:
                    cur_ret = argty(cur_ret, atom.ret_ty)
                return atom.ret_ty, replace (pipe, args=new_args, return_var=cur_ret)
        # case: sequence of two pipes. Only the return of the second pipe is used.
        case Seq(left, right):
            _, left = type_pipeline_vars(None, left)
            ret_ty, right = type_pipeline_vars(cur_ret, right)
            return ret_ty, replace(pipe, left=left, right=right)
        # case: let binding. The return variable of the left pipe is bound to the name in the let binding.
        case Let(ret, left, right):
            left_ret_ty, left = type_pipeline_vars(ret, left)
            ret = argty(ret, left_ret_ty)
            right_ret_ty, right = type_pipeline_vars(cur_ret, right)
            return right_ret_ty, replace(pipe, ret=ret, left=left, right=right)
        # case: change the pipe's location -- this has no effect on the return variables
        case Exit(_):
            if (cur_ret == None):
                return None, pipe
            else:
                print("Error: exit pipe in a pipe that expects a return value from it.")
                exit(1)
        case Switch(_, cases):
            new_cases = {}
            ret_tys = []
            for k, v in cases.items():
                ret_ty, new_case = type_pipeline_vars(cur_ret, v)
                ret_tys.append(ret_ty)
                new_cases[k] = new_case
            # make sure all the ret_tys are identical
            for i in range(1, len(ret_tys)):
                if (ret_tys[i] != ret_tys[0]):
                    print("Error: switch statement with different return types in each case.")
                    exit(1)
            return ret_tys[0], replace(pipe, cases=new_cases)
        case At(_, inner_pipe):
            ret_ty, inner_pipe = type_pipeline_vars(cur_ret, inner_pipe)
            return ret_ty, replace(pipe, inner_pipe=inner_pipe)
        case MainPipe(_, pipes):
            new_pipes = []
            for k, v in pipes:
                ret_ty, new_pipe = type_pipeline_vars(cur_ret, v)
                new_pipes.append((k, new_pipe))
            return None, replace(pipe, pipes=new_pipes)
        


def switch_continuations(pipe : PipeBase):
    """
        before we can do location analysis, we need to inline switch expression continuations. 
        that means, if there is a sequence or let where the left side is a switch, 
        we need to inline the right side (the continuation) into each of the cases.  
        This should be done before location analysis.
        example:
        seq(switch(var, cases), right) ==> switch(var, {k: seq(cases[k], right) for k in cases})

    """
    match pipe:
        case Seq(left, right):
            new_right = switch_continuations(right)
            if (isinstance(left, Switch)):
                new_cases = {k: Seq(left=switch_continuations(v), right=new_right) for k, v in left.cases.items()}
                return replace(left, cases=new_cases)
            else:
                return replace(pipe, left=switch_continuations(left), right=new_right)
        case Let(ret, left, right):
            new_right = switch_continuations(right)
            if (isinstance(left, Switch)):
                new_cases = {k: Let(ret=ret, left=v, right=new_right) for k, v in left.cases.items()}
                return replace(left, cases=new_cases)
            else:
                return replace(pipe, left=switch_continuations(left), right=new_right)
        case At(_, inner_pipe):
            return replace(pipe, inner_pipe=switch_continuations(inner_pipe))
        case Switch(_, cases):
            new_cases = {k: switch_continuations(v) for k, v in cases.items()}
            return replace(pipe, cases=new_cases)
        case Atom(_, _, _, _):
            return pipe
        case Exit(_):
            return pipe
        case _: 
            return recurse(switch_continuations, pipe)

def at_continuations(pipe : PipeBase):
    """
        Move the successor of an "at" operation into the "at". 
        example: 
            seq(at(1, x), y) ==> at(1, seq(x, y))
        This is designed to run after switch_continuations, 
        but can probably come anywhere before location analysis.
    """
    match pipe:
        case Atom(_,_,_,_):
            return pipe
        case Exit(_):
            return pipe
        case At(_, inner_pipe): 
            return replace(pipe, inner_pipe=at_continuations(inner_pipe))
        case Switch(_, cases):
            new_cases = {k: at_continuations(v) for k, v in cases.items()}
            return replace(pipe, cases=new_cases)
        case Seq(left, right):
            left = at_continuations(left)
            right = at_continuations(right)
            if (isinstance(left, At)):
                return replace(left, inner_pipe=Seq(left=left.inner_pipe, right=right))
            else:
                return replace(pipe, left=left, right=right)
        case Let(var, left, right):
            left = at_continuations(left)
            right = at_continuations(right)
            if (isinstance(left, At)):
                return replace(left, inner_pipe=Let(ret=var, left=left.inner_pipe, right=right))
            else:
                return replace(pipe, left=left, right=right)
        case _:
            return recurse(at_continuations, pipe)

def double_at_elimination(pipe):
    """
        Optimization: Eliminate at-at sequences.
        example:
            at(c1, at(c2, x)) ==> at(c2, x)
    """
    match pipe:
        case Atom(_, _, _, _):
            return pipe
        case Exit(_):
            return pipe
        case At(loc, inner_pipe):
            inner_pipe = double_at_elimination(inner_pipe)
            if (isinstance(inner_pipe, At)):
                return inner_pipe
            else:
                return replace(pipe, inner_pipe=inner_pipe)
        case Seq(left, right):
            left = double_at_elimination(left)
            right = double_at_elimination(right)
            return replace(pipe, left=left, right=right)
        case Let(var, left, right):
            left = double_at_elimination(left)
            right = double_at_elimination(right)
            return replace(pipe, left=left, right=right)
        case Switch(var, cases):
            new_cases = {k: double_at_elimination(v) for k, v in cases.items()}
            return replace(pipe, cases=new_cases)
        case _:
            return recurse(double_at_elimination, pipe)
        
def locate_pipes(pipe : PipeBase, cur_loc : str):
    """
        Annotate each pipe with a start and end location.
    """
    match pipe:
        case Atom(_, _, _, _):
            return replace (pipe, start=cur_loc, end=cur_loc)
        case Seq(left, right):
            # a sequence starts at the start of the left pipe, and ends at the end of the right pipe
            # the left pipe's end is the right pipe's start
            if (isinstance(left, Switch)):
                # it should be impossible to have a switch start a sequence, 
                # because we inline switch continuations
                raise Exception("Got a switch at the start of a sequence, this should not happen because of the inline_switch_continuations pass.")
            new_left = locate_pipes(left, cur_loc)
            new_right = locate_pipes(right, new_left.end)
            seq_start = new_left.start
            seq_end = new_right.end            
            return replace(pipe, left=new_left, right=new_right, start=seq_start, end=seq_end)
        case Let(_, left, right):
            # a let pipe has the same location rules as a sequence
            if (isinstance(left, Switch)):
                # it should be impossible to have a switch start a sequence, 
                # because we inline switch continuations
                raise Exception("Got a switch at the start of a let, this should not happen because of the inline_switch_continuations pass.")
            new_left = locate_pipes(left, cur_loc)
            new_right = locate_pipes(right, new_left.end)
            seq_start = new_left.start
            seq_end = new_right.end
            return replace(pipe, left=new_left, right=new_right, start=seq_start, end=seq_end)
        case At(location, inner_pipe):
            # an at pipe changes the location of the inner pipe
            # its own location starts at the current location, and ends at the specified location
            new_inner = locate_pipes(inner_pipe, location)
            return replace(pipe, inner_pipe=new_inner, start=cur_loc, end=new_inner.end)
        case Exit(_):
            # TODO: design choice: should an exit change the location of the pipe?
            return replace(pipe, start=cur_loc, end=cur_loc)
        case Switch(_, cases):
            # a switch pipe starts here, but has no end location unless all the 
            # cases have the same end location
            new_cases = {k: locate_pipes(v, cur_loc) for k, v in cases.items()}
            case_end_locs = [v.end for v in new_cases.values()]
            multi_end_loc = "( " + " or ".join(case_end_locs) + " )"
            if (len(set(case_end_locs)) == 1):
                return replace(pipe, cases=new_cases, start=cur_loc, end=case_end_locs[0])
            else:
                return replace(pipe, cases=new_cases, start=cur_loc, end=multi_end_loc)
        case MainPipe(_, pipes):
            # a nic d
            if (cur_loc != start_loc):
                print("Error: MainPipe pipe in a non-start location.")
                exit(1)
            new_pipes = [(k, locate_pipes(v, cur_loc)) for k, v in pipes]
            pipe_end_locs = [v.end for _, v in new_pipes]
            if (len(set(pipe_end_locs)) == 1):
                return replace(pipe, pipes=new_pipes, start=cur_loc, end=pipe_end_locs[0])
            else:
                return replace(pipe, pipes=new_pipes, start=cur_loc, end="( " + " or ".join(pipe_end_locs) + " )")
        

def self_at_elimination(pipe):
    """
        Optimization: Eliminate at nodes that move to the same location.
        example: 
            at<start=loc, end=...>(loc, x) ==> x
    """
    match pipe:
        case At(loc, inner_pipe):
            inner_pipe = self_at_elimination(inner_pipe)
            if pipe.start == loc:
                return inner_pipe
            else:
                return replace(pipe, inner_pipe=inner_pipe)
        case _:
            return recurse(self_at_elimination, pipe)
            
def explicit_packet_args(pipe):
    """add an explicit packet argument to each atom"""
    match pipe:
        case Atom(_, atom, args, ret):
            return replace(pipe, args=[packet_arg]+args)
        case _: return recurse(explicit_packet_args, pipe)

def frontend_passes(pipe):
    # 1. instantiate the pipe (name for state variables)
    ip = instantiate(pipe)
    # 2. names for variables
    ip = rename_vars({}, ip)
    # 3. bind return variables to atoms
    _, ip = type_pipeline_vars(None, ip)
    # 4. inline switch and at continuations
    ip = switch_continuations(ip)
    ip = at_continuations(ip)
    ip = double_at_elimination(ip)
    # 5. location analysis
    ip = locate_pipes(ip, start_loc)
    ip = self_at_elimination(ip)
    # 6. add explicit packet arguments (this can happen anywhere)
    ip = explicit_packet_args(ip)
    # program should now be ready for conversion to IR in backend.py
    return ip

