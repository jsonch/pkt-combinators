#! /usr/bin/env python3
# minimal python prototype of combinator compiler
from dataclasses import dataclass
import re
import os, sys
import io
import contextlib

#### AST
@dataclass
class AtomDecl:
    """
    An atom is a function with some state, we need to know the 
    state type and return variable type to generate the correct
    initialization code. We need to know the function name to
    generate the correct function call.

    The signature of an atom's function should be: 
    state_ty -> params -> ret_ty -> ()
    (everything should be a reference type / pointer in c)
    """
    state_ty : str
    init_name : str
    ret_ty : str
    fn_name : str
    __match_args__ = ("state_ty", "init_name", "ret_ty", "fn_name")

class PipeBase:
    """
    A pipe is a function that can be applied to a packet. It is 
    built using combinators defined below.
    """
    pass

@dataclass
class Atom(PipeBase):
    """
    An atom pipe is a pipe containing a single atom, 
    called with the given arguments.
    """
    state_name : str = None
    atom : AtomDecl = None
    args : list[str] = None
    return_var : str = None
    __match_args__ = ("state_name", "atom", "args", "return_var")

@dataclass
class Seq(PipeBase):
    """
    Two pipes can be sequenced together to form a new pipe, 
    where the output of one pipe is the input of the other.
    """
    left : PipeBase
    right : PipeBase
    __match_args__ = ("left", "right")

@dataclass
class Let(PipeBase):
    """
    Two pipes can be sequenced together with a let binding, 
    which gives a name to the return value of the first pipe 
    that can be used in the second pipe.
    """
    return_name : str
    left : PipeBase
    right : PipeBase
    __match_args__ = ("return_name", "left", "right")


#### BUILTINS
default_state_arg = "_no_state"
packet_arg = "pkt"

global idctr; idctr = -1

#### atom helpers
def has_state(atom : AtomDecl):
    return atom.state_ty is not None
def name(atom : AtomDecl):
    return atom.fn_name
def ret_ty(atom : AtomDecl):
    return atom.ret_ty


def fresh_name(name):
    global idctr
    idctr = idctr + 1
    return f"{name}_{idctr}"



#### Transformation passes
def instantiate(pipe : PipeBase):
    """
    instantiate a unique instance of each atom in the pipe, 
    by giving each atom a unique state variable name.
    TODO: this will change when we add the "shared" combinator
    """
    match pipe: 
        case Atom(None, atom, args, return_var):
            if (has_state(atom)):
                return Atom(fresh_name(name(atom)+"_state"), atom, args, return_var)
            else:
                return Atom(None, atom, args, return_var)
        case Seq(left, right):
            return Seq(instantiate(left), instantiate(right))
        case Let(return_name, left, right):
            return Let(return_name, instantiate(left), instantiate(right))
            
def rename_vars(renames : dict[str, str], pipe : PipeBase):
    """rename variables in let bindings to prevent shadowing"""
    match pipe:
        case Atom(state_id, atom, args, return_var):
            renamed_args = []
            for arg in args:
                if arg in renames:
                    renamed_args.append(renames[arg])
                else:
                    raise Exception(f"Unbound variable {arg}")
            return Atom(state_id, atom, renamed_args, return_var)
        case Seq(left, right):
            # the return variable gets bound to the output of the last (right) pipe
            return Seq(rename_vars(renames, left), rename_vars(renames, right))
        case Let(return_name, left, right):
            fresh_return_name = fresh_name(return_name)
            inner_renames = {r: f for r, f in renames.items() if r != return_name}
            inner_renames = {**inner_renames, **{return_name: fresh_return_name}} # add the fresh return name
            return Let(fresh_return_name, rename_vars(renames, left), rename_vars(inner_renames, right))

def assign_return_vars(return_var : str, pipe : PipeBase):
    """set each atom's return variable. Must be run _after_ rename_vars. """
    match pipe:
        case Atom(state_id, atom, args, None):
            if (return_var == None):
                return_var = fresh_name("_unused"+name(atom)+"_ret")
            return Atom(state_id, atom, args, return_var)
        case Seq(left, right):
            return Seq(assign_return_vars(None, left), assign_return_vars(return_var, right))
        case Let(return_name, left, right):
            return Let(return_name, assign_return_vars(return_name, left), assign_return_vars(return_var, right))


#### Code generators
### 1. global declarationsdeclaration of globals
### 2. local declarations declaration of locals (return variables)
### 3. body of handler function that applies the pipe to a packet

def gen_global_decls(pipe : PipeBase):
    """declare all the state variables for the pipe"""
    match pipe:
        case Atom(None, atom, args):
            if (not has_state(atom)):
                return ""
            else:
                raise Exception("Uninitialized stateful atom")
        case Atom(state_name, atom, args):
            match atom:
                case AtomDecl(None, None, _, _):
                    return "" # no initialization
                case _: 
                    return f"{atom.state_ty} {state_name} = "+'{0};'
        case Seq(left, right):
            left_str = gen_global_decls(left)
            right_str = gen_global_decls(right)
            if (left_str == "" or right_str == ""):
                return f"{left_str}{right_str}"
            else:
                return f"{left_str}\n{right_str}"
        case Let(_, left, right):
            left_str = gen_global_decls(left)
            right_str = gen_global_decls(right)
            if (left_str == "" or right_str == ""):
                return f"{left_str}{right_str}"
            else:
                return f"{left_str}\n{right_str}"


def gen_local_decls(pipe : PipeBase):
    """generate the local variable declarations for the pipe
    (every atom's return variable, which also covers every atom's args)"""
    match pipe:
        case Atom(state_id, atom, args, return_var):
            return f"{ret_ty(atom)} {return_var};"
        case Seq(left, right):
            return f"{gen_local_decls(left)}\n{gen_local_decls(right)}"
        case Let(_, left, right):
            return f"{gen_local_decls(left)}\n{gen_local_decls( right)}"    

def gen_calls(pipe : PipeBase):
    """ generate the calls to atoms in the pipe """
    match pipe:
        case Atom(state_id, atom, args, return_var):
            state_arg = default_state_arg            
            if (state_id != None): # dummy state var
                state_arg = state_id
            args = ["&"+arg for arg in args] # pass by reference
            args = ["&"+state_arg, packet_arg] + args + ["&"+return_var] # add state, packet, and return arg
            return f"{name(atom)}({', '.join(args)});"
        case Seq(left, right):
            return f"{gen_calls(left)}\n{gen_calls(right)}"
        case Let(inner_return_var, left, right):
            return f"{gen_calls(left)}\n{gen_calls(right)}"


def tab(n : int, s : str):
    return "\n".join([f"{' '*n}{line}" for line in s.split("\n")])

def test_main():
    return """int main() {
    char pkt[] = "\\x00\\x01\\x02\\x03\\x04\\x05\\x06\\x07\\x08\\x09\\x0a\\x0b\\x0c\\x0d\\x0e\\x0f";
    handle_packet(pkt);
    printf ("--- final packet ---\\n");
    for (int i = 0; i < sizeof(pkt); i++) {
      printf("%02x ", (unsigned char)pkt[i]);
    }
    printf("\\n");
}
"""
#### Toplevel compiler function
def compile(hdl_name : str, pipe : PipeBase):
    # 1. instantiate the pipe (name for state variables)
    ip = instantiate(pipe)
    # 2. names for variables
    ip = rename_vars({}, ip)
    # 3. bind return variables to atoms
    ip = assign_return_vars(None, ip)
    # 4. generate the code blocks
    global_decls = gen_global_decls(ip)
    local_decls = tab(2, gen_local_decls(ip))
    calls = tab(2, gen_calls(ip))
    # builtins
    default_state_decl = "int _no_state = 0;"
    global_decls = f"{default_state_decl}\n{global_decls}"
    handler_function = f"void {hdl_name}(char* {packet_arg}) {{\n{local_decls}\n{calls}\n}}"
    return ("//Globals\n"+global_decls + "\n//Handler\n" + handler_function)

#### User-facing combinator functions
def atom(state_ty : str, ret_ty : str, fn_name : str):
    return AtomDecl(state_ty, None, ret_ty, fn_name)

def pipe(atom : AtomDecl, args : list[str]):
    return Atom(None, atom, args)

def seq(left : PipeBase, right : PipeBase):
    return Seq(left, right)

def let(return_name : str, left : PipeBase, right : PipeBase):
    return Let(return_name, left, right)

def test():
    # test case: 
    # a pipe that parses a packet, counts the number of packets, and forwards the packet
    parse_atom = atom(None, "parsed_pkt_t", "parse")
    counter_atom = atom("counter_state_t", "counter_ret_t", "counter")
    fwd_atom = atom("fwd_state_t", "fwd_ret_t", "fwd")

    p1 =let("parsed_pkt", pipe(parse_atom, []),
        let("ct", pipe(counter_atom, ["parsed_pkt"]),
        let("ct", pipe(counter_atom, ["parsed_pkt"]),    
        pipe(fwd_atom, ["parsed_pkt", "ct"]
    ))))
    # let "parsed_pkt" = pipe(parse_atom)() in
    # counter1 = counter_atom("parsed_pkt")
    # p1 = let "parsed_pkt" = parse()


    prog_string = compile("p1", p1)  
    print(prog_string) 


def preprocess(src_fn: str):
    # preprocess src_py.c to src.c, 
    # replacing all the PYTHON({...}) blocks with the 
    # print outputs of the python code
    base_name = os.path.basename(src_fn)
    dst_fn = os.path.splitext(base_name)[0].rstrip("_py") + ".c"
    print(f"Preprocessing {src_fn} to {dst_fn}.c")
    # Rest of the preprocessing logic goes here
    with open(src_fn, 'r') as f:
        src = f.read()
    def repl_python(match):
        code = match.group(1)
        # print(f"Running python code: {code}")
        buffer = io.StringIO()
        with contextlib.redirect_stdout(buffer):
            exec(code, globals())
        # print(f"Output: {buffer.getvalue()}")
        orig_str = "/* PYTHON({"+code+"}) */"
        if buffer.getvalue() == "":
            return orig_str
        else:
            return orig_str + "\n//GENERATED CODE:\n" + buffer.getvalue()        
    dst = re.sub(r"PYTHON\({(.*?)}\)", repl_python, src, flags=re.DOTALL)
    with open(dst_fn, 'w') as f:
        f.write(dst)

if __name__ == '__main__':
    # call preprocess on the first arg
    if len(sys.argv) != 2:
        print("Usage: mario.py <source>")
        exit(1)
    src_fn = sys.argv[1]
    preprocess(src_fn)
