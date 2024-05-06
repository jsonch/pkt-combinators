# frontend syntax for mario

from dataclasses import dataclass, replace
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
class PipeBase:
    """
    A pipe is a function that can be applied to a packet. It is 
    built using combinators defined below.
    Every pipe has a start and end location.
    """
    start : str = None
    end : str = None

@dataclass(frozen=True)
class Atom(PipeBase):
    """
    An atom pipe is a pipe containing a single atom, 
    called with the given arguments.
    """
    state : str = None
    atom : AtomDecl = None
    args : list[str] = None
    return_var : str = None
    __match_args__ = ("state", "atom", "args", "return_var")

@dataclass(frozen=True)
class Seq(PipeBase):
    """
    Two pipes can be sequenced together to form a new pipe, 
    where the output of one pipe is the input of the other.
    """
    left : PipeBase = None
    right : PipeBase = None
    __match_args__ = ("left", "right")

@dataclass(frozen=True)
class Let(PipeBase):
    """
    Two pipes can be sequenced together with a let binding, 
    which gives a name to the return value of the first pipe 
    that can be used in the second pipe.
    """
    return_name : str = None
    left : PipeBase = None
    right : PipeBase = None
    __match_args__ = ("return_name", "left", "right")

@dataclass(frozen=True)
class At(PipeBase):
    """
    A pipe can be applied to a packet at a specific location.
    """
    location : str = None
    inner_pipe : PipeBase = None
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
    dest : str = None
    __match_args__ = ("dest",)


#### User-facing combinators

## atom constructors
def atom(state_ty : str, ret_ty : str, fn_name : str, arg_tys : list[str]):
    """declare an atom"""
    return AtomDecl(state_ty, ret_ty, fn_name, arg_tys)

## pipe combinators
def do(atom : AtomDecl, args : list[str], state: str = None):
    """
        turn an atom into a pipe by running it
        if state name is given, the atom is run using the indicated 
        state variable, which may be shared. If no state name is given,
        each instance of the atom gets its own non-shared state variable.                        
    """
    return Atom(state=state, atom=atom, args=args)

def seq(left : PipeBase, right : PipeBase):
    return Seq(left=left, right=right)

def let(return_name : str, left : PipeBase, right : PipeBase):
    return Let(return_name=return_name, left=left, right=right)

def at(location : str, inner_pipe : PipeBase):
    return At(location=location, inner_pipe=inner_pipe)

def drop():
    return Exit()

def fwd(dest : str):
    return Exit(dest=dest)



#### printers
def nindent(n):
    return "\n" + " " * n

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
            return f"seq({pipe_to_string(left)},{nindent(indent)}{pipe_to_string(right)})"
        case Let(return_name, left, right):
            return f"let({return_name}, {pipe_to_string(left)},{nindent(indent)}{pipe_to_string(right, indent=indent)})"
        case At(location, inner_pipe):
            return f"at({location},{nindent(indent+2)}{pipe_to_string(inner_pipe, indent=indent+2)}"
        case Exit(dest):
            if dest is not None:
                return f"fwd({dest})"
            else:
                return "drop()"
        

def pretty_print(pipe : PipeBase, indent=0):
    """pretty-print pipe with nicer syntax, but not python-parseable"""
    match pipe:
        case Atom(state, atom, args, return_var):
            return f"do {atom.fn_name}({state}, {args})"
        case Seq(left, right):
            return f"{pretty_print(left, indent)}\n{pretty_print(right,indent)}"
        case Let(return_name, left, right):
            left_str = pretty_print(left, indent)
            right_str = pretty_print(right, indent)
            if (left_str.count("\n") > 1):
                left_str = nindent(indent+2) + left_str
                return f"let {return_name} = {left_str}{nindent(indent+2)}in{nindent(2)}{right_str}"
            else:
                return f"let {return_name} = {left_str} in{nindent(indent)}{right_str}"
        case At(location, inner_pipe):
            return f"at {location} {pretty_print(inner_pipe, indent+2)}"
        case Exit(dest):
            if dest is not None:
                return f"fwd {dest}"
            else:
                return "drop"

def pretty_print_located(pipe : PipeBase, indent=0):
    """pretty-print pipe with nicer syntax, but not python-parseable"""
    def loc_tag(pipe):
        if pipe.start is None and pipe.end is None:
            return ""
        elif(pipe.start is None and pipe.end is not None):
            print(f"Warning: pipe {pipe} has an end location but no start location")
            return f"<<start=???, end={pipe.end}>>"
        elif(pipe.start is not None and pipe.end is None):
            print(f"Warning: pipe {pipe} has a start location but no end location")
            return f"<<start={pipe.start}, end=???>>"
        elif(pipe.start == pipe.end):
            return f"<<loc={pipe.start}>>"
        else:   
           return f"<<start={pipe.start}, end={pipe.end}>>"
    match pipe:
        case Atom(state, atom, args, return_var):
            return f"run{loc_tag(pipe)} {atom.fn_name}({state}, {args})"
        case Seq(left, right):
            return f"{pretty_print_located(left, indent)}{nindent(indent)}{pretty_print_located(right,indent)}"
        case Let(return_name, left, right):
            left_str = pretty_print_located(left, indent)
            right_str = pretty_print_located(right, indent)
            if (left_str.count("\n") > 1):
                left_str = nindent(indent+2) + left_str
                return f"let {return_name} = {left_str}{nindent(indent+2)}in{nindent(2)}{right_str}"
            else:
                return f"let {return_name} = {left_str} in{nindent(indent)}{right_str}"
        case At(location, inner_pipe):
            return f"at{loc_tag(pipe)} {location} do{nindent(indent+2)}{pretty_print_located(inner_pipe, indent+2)}"
        case Exit(dest):
            if dest is not None:
                return f"fwd{loc_tag(pipe)}({dest})"
            else:
                return "drop{loc_tag(pipe)}()"



def located_pipe_to_string(pipe : PipeBase, indent=0):
    match pipe:
        case Atom(_):
            pipe_str = pipe_to_string(pipe, indent=indent)
            return f"<<start={pipe.start}, end={pipe.end}>>{pipe_str}"
        case Seq(left, right):
            left_str = located_pipe_to_string(left, indent=indent)
            right_str = located_pipe_to_string(right, indent=indent)
            return f"<<start={pipe.start}, end={pipe.end}>>seq({left_str},{nindent(indent)}{right_str})"
        case Let(return_name, left, right):
            left_str = located_pipe_to_string(left, indent=indent)
            right_str = located_pipe_to_string(right, indent=indent)
            return f"<<start={pipe.start}, end={pipe.end}>>let({return_name}, {left_str},{nindent(indent)}{right_str})"
        case At(location, inner_pipe):
            inner_str = located_pipe_to_string(inner_pipe, indent=indent+2)
            return f"<<start={pipe.start}, end={pipe.end}>>at({location},{nindent(indent+2)}{inner_str})"
        case Exit(dest):
            if dest is not None:
                return f"<<start={pipe.start}, end={pipe.end}>>fwd({dest})"
            else:
                return f"<<start={pipe.start}, end={pipe.end}>>drop()"
