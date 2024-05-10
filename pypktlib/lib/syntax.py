# frontend syntax for mario
from collections import namedtuple
from dataclasses import dataclass, replace
from typing import TypeVar, Optional

#### AST
@dataclass
class AtomDecl:
    """
    An atom is a function with some state, we need to know the 
    state type and return variable type to generate the correct
    initialization code. We need to know the function name to
    generate the correct function call.

    The signature of an atom's function should be:
    state_ty -> packet_ty -> params -> ret_ty -> ()
    
    note: all arguments should be pointers
    """
    state_ty : str
    init_fn_name  : str
    ret_ty : str
    fn_name : str
    arg_tys : list[str]
    __match_args__ = ("state_ty", "ret_ty", "fn_name", "arg_tys")

#### atom helpers
def has_state(atom : AtomDecl):
    return atom.state_ty is not None
def name(atom : AtomDecl):
    return atom.fn_name
def ret_ty(atom : AtomDecl):
    return atom.ret_ty

@dataclass(frozen=True)
class Var:
    """
    A variable is a name with an optional type annotation.
    """
    name : Optional[str] = None
    ty : Optional[str] = None
    __match_args__ = ("name", "ty")
    def __str__(self):
        if self.ty is not None:
            return f"{self.name}:{self.ty}"
        else:
            return f"{self.name}"


def var(name : str):
    return Var(name, None)
def varty(name : str, ty : str):
    return Var(name, ty)

def is_pointer(ty : str):
    return ty.strip().endswith("*")
def ref(var : Var):
    if is_pointer(var.ty):
        return f"{var.name}"
    else:
        return f"&{var.name}"


@dataclass(frozen=True)
class NicPort:
    """
    A NIC port is a pair of device and queue number.
    """
    dev : str = None
    devnum : int = None
    queue : int = None
    __match_args__ = ("dev", "queue")
    def __str__(self):
        return f"{self.dev}(@queue={self.queue})"    



@dataclass(frozen=True)
class PipeBase:
    """
    A pipe is a function that can be applied to a packet. It is 
    built using combinators defined below.
    Every pipe has a start and end location.
    """
    start : Optional[str] = None
    end : Optional[str] = None

@dataclass(frozen=True)
class Atom(PipeBase):
    """
    An atom pipe is a pipe containing a single atom, 
    called with the given arguments.
    """
    # state : str = None
    state : Optional[Var] = None
    atom : Optional[AtomDecl] = None
    args : Optional[list[Var]] = None
    return_var : Optional[Var] = None
    __match_args__ = ("state", "atom", "args", "return_var")

@dataclass(frozen=True)
class Seq(PipeBase):
    """
    Two pipes can be sequenced together to form a new pipe, 
    where the output of one pipe is the input of the other.
    """
    left : Optional[PipeBase] = None
    right : Optional[PipeBase] = None
    __match_args__ = ("left", "right")

@dataclass(frozen=True)
class Let(PipeBase):
    """
    Two pipes can be sequenced together with a let binding, 
    which gives a name to the return value of the first pipe 
    that can be used in the second pipe.
    """
    ret : Optional[Var] = None
    left : Optional[PipeBase] = None
    right : Optional[PipeBase] = None
    __match_args__ = ("ret", "left", "right")

@dataclass(frozen=True)
class At(PipeBase):
    """
    A pipe can be applied to a packet at a specific location.
    """
    location : Optional[str] = None
    inner_pipe : Optional[PipeBase] = None
    __match_args__ = ("location","inner_pipe")

@dataclass(frozen=True)
class Exit(PipeBase):
    """
    A pipe that exits processing, optionally sending the packet 
    to a specific destination. Note that, this destination is 
    different from a location, which is a place where processing 
    happens. This location is an output device, like "eth0".
    If no destination is given, the packet is dropped.
    """
    dest : Optional[NicPort] = None
    __match_args__ = ("dest",)

@dataclass(frozen=True)
class Switch(PipeBase):
    """
    A pipe that switches on a bound variable, executing the 
    pipe corresponding to the value of the variable.
    """
    var : Optional[Var] = None
    cases : Optional[dict[int, PipeBase]] = None
    __match_args__ = ("var", "cases")

@dataclass(frozen=True)
class NicDeliver(PipeBase):
    """deliver a packet from the network to one or more pipes based on some policy.
       The "policy" is just an uninterpreted string for now. """    
    policy  : Optional[str] = None
    pipes : Optional[list[(NicPort,PipeBase)]] = None
    __match_args__ = ("policy", "pipes")  
    def __str__(self):
        pipemap_strs = '\n  '.join([f"{str(nicport)} -> {pretty_print(pipe, indent=2)}"for (nicport, pipe) in self.pipes])
        return f"NicDeliver(\n  {pipemap_strs}\n)"
    

#### builtin constant variables
start_loc = "network"
default_state_arg = Var("_no_state", "char *")
packet_arg = Var("pkt", "char *")


#### helpers
global idctr; idctr = -1
def fresh_name(name):
    global idctr
    idctr = idctr + 1
    return f"{name}_{idctr}"

def fresh_var(var):
    return varty(fresh_name(var.name), var.ty)


#### printers
def newtab(n):
    return "\n" + " " * n
def tablines(n, line):
    return (" " * n)+ newtab(n).join(line.split("\n"))

def atom_to_string(atom : AtomDecl):
    if atom.state_ty is not None:
        return f"atom({atom.state_ty}, {atom.ret_ty}, {atom.fn_name})"
    else:
        return f"atom(None, {atom.ret_ty}, {atom.fn_name})"
    
def pipe_to_string(pipe : PipeBase, indent = 0):
    """print a pipe as a python string"""
    match pipe:
        case Atom(state, atom, args, return_var):
            if state is not None:
                return f"do({atom_to_string(atom)}, {args}, {state})"
            else:
                return f"do({atom_to_string(atom)}, {args})"
        case Seq(left, right):
            return f"seq({pipe_to_string(left)},{newtab(indent)}{pipe_to_string(right)})"
        case Let(return_name, left, right):
            return f"let({return_name}, {pipe_to_string(left)},{newtab(indent)}{pipe_to_string(right, indent=indent)})"
        case At(location, inner_pipe):
            return f"at({location},{newtab(indent+2)}{pipe_to_string(inner_pipe, indent=indent+2)}"
        case Exit(dest):
            if dest is not None:
                return f"fwd({dest})"
            else:
                return "drop()"
        case Switch(var, cases):
            case_strs = [f"{k}: {pipe_to_string(v)}" for k, v in cases.items()]
            return f"switch({var},{{\n{newtab(indent+2).join(case_strs)}\n{newtab(indent)}}})"

def pretty_print(pipe : PipeBase, indent=0):
    """pretty-print, annotated with locations """
    def loc_tag(pipe):
        if not pipe.start and not pipe.end:
            return ""
        elif(not pipe.start and pipe.end):
            print(f"Warning: pipe {pipe} has an end location but no start location")
            return f"<<start=???, end={pipe.end}>>"
        elif(pipe.start and not pipe.end):
            print(f"Warning: pipe {pipe} has a start location but no end location")
            return f"<<start={pipe.start}, end=???>>"
        elif(pipe.start == pipe.end):
            return f"<<loc={pipe.start}>>"
        else:   
           return f"<<start={pipe.start}, end={pipe.end}>>"
    match pipe:
        case Atom(state, atom, args, return_var):
            arg_str = ", ".join([str(arg) for arg in args])            
            return f"run{loc_tag(pipe)} {atom.fn_name}(state={state}, {arg_str})"
        case Seq(left, right):
            return f"{pretty_print(left, indent)}{newtab(indent)}>> {pretty_print(right,indent)}"
        case Let(ret, left, right):
            left_str = pretty_print(left, indent)
            right_str = pretty_print(right, indent)
            do_newblock = False
            if (left_str.count("\n") > 1 or len(left_str) > 32):
                do_newblock = True
            if (do_newblock):
                return f"let {ret} = {newtab(indent+2)}{left_str}{newtab(indent)}in{newtab(indent)}{right_str}"
            else:
                return f"let {ret} = {left_str} in{newtab(indent)}{right_str}"
        case At(location, inner_pipe):
            return f"at{loc_tag(pipe)} {location} do{newtab(indent+2)}{pretty_print(inner_pipe, indent+2)}"
        case Exit(dest):
            if dest is not None:
                return f"fwd{loc_tag(pipe)}({dest})"
            else:
                return f"drop{loc_tag(pipe)}()"
        case Switch(var, cases):
            case_strs = [f"{k}:({newtab(indent+4)}{pretty_print(v, indent+4)}{newtab(indent+2)})" for k, v in cases.items()]
            indent_str = newtab(indent+2)
            return f"switch{loc_tag(pipe)} {var} [{indent_str}{indent_str.join(case_strs)}{newtab(indent)}]"
        case _:
            return str(pipe)


#### User-facing combinators
## atom constructors
def atom(state_ty : str, state_init: str, ret_ty : str, f : str, arg_tys : list[str]):
    """declare an atom"""
    return AtomDecl(state_ty, state_init, ret_ty, f, arg_tys)

## pipe constructors
def do(atom : AtomDecl, args : list[str], state: Optional[str] = None):
    """
        Run an atom on its state, the packet variable (implicit), and any 
        pipeline variables (args).
    """
    arg_vars = [var(arg) for arg in args]
    if (state):
        return Atom(state=var(state), atom=atom, args=arg_vars)
    else:
        return Atom(state=None, atom=atom, args=arg_vars)

def seq(left : PipeBase, right : PipeBase):
    return Seq(left=left, right=right)

def let(return_name : str, left : PipeBase, right : PipeBase):
    return Let(ret=var(return_name), left=left, right=right)

def at(location : str, inner_pipe : PipeBase):
    return At(location=location, inner_pipe=inner_pipe)

def drop():
    return Exit()

def forward(dest : tuple[int, int]):
    dest = NicPort("", dest[1], dest[0])
    return Exit(dest=dest)

def switch(v : str, cases : dict[int, PipeBase]):
    return Switch(var=var(v), cases=cases)

def nic(pipes):
    global next_devnum
    # convert (dev, queue) pairs to NicPort objects
    nicport_pipes = []
    for (dev, queue), pipe in pipes:
        nicport_pipes.append((NicPort("", queue, dev), pipe))
    return NicDeliver(policy=None, pipes=nicport_pipes)
