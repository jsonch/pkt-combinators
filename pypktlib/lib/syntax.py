# frontend syntax for mario
from collections import namedtuple
from dataclasses import dataclass, replace
from typing import TypeVar, Optional
from string import Template
import re

#### AST

@dataclass(frozen=True)
class Ty():
    pass

@dataclass(frozen=True)
class TyName(Ty):
    """A type name and optional definition."""
    name : Optional[str] = None
    cstr : Optional[str] = None
    __match_args__ = ("name")
    def __str__(self):
        return self.name
    def c_str(self):
        return self.cstr

@dataclass(frozen=True)
class Ptr(Ty):
    """A pointer to an inner type."""
    inner_ty : Optional[Ty] = None
    __match_args__ = ("inner_ty")
    def __str__(self):
        rv = f"{self.inner_ty}"
        return rv
    def c_str(self):
        if (self.inner_ty != None):
            return self.inner_ty.c_str()

@dataclass(frozen=True)
class ArgExp():
    """An argument to an atom"""
    def base_name(self):
        match self:
            case CompoundVar(base, _):
                return base.base_name()
            case _:
                return self.name
    def base_rename(self, new_name):
        match self:
            case CompoundVar(base, _):
                return replace(self, base=base.base_rename(new_name))
            case _:
                return replace(self, name=new_name)

@dataclass(frozen=True)
class Var(ArgExp):
    """
    A variable is a name with an optional type annotation.
    """
    name : Optional[str] = None
    ty : Optional[Ty] = None
    __match_args__ = ("name", "ty")
    def __str__(self):
        if self.ty is not None:
            return f"{self.name}:{self.ty}"
        else:
            return f"{self.name}"
    def namestr(self):
            return f"{self.name}"

@dataclass(frozen=True)
class CompoundVar(ArgExp):
    """A compound variable is a field of a struct"""
    base : Optional[ArgExp] = None
    fieldname : Optional[str] = None
    ty : Optional[Ty] = None
    __match_args__ = ("base", "fieldname", "ty")
    def __str__(self):
        return f"{self.base}.{self.fieldname}"
    def namestr(self):
        return f"{self.base.namestr()}.{self.fieldname}"
    
@dataclass(frozen=True)
class Val(ArgExp):
    """ A value is an int constant. """
    value : int
    ty : Optional[Ty]
    __match_args__ = ("value", "ty")
    def __str__(self):
        return str(self.value)
    @classmethod
    def from_int(cls, i):
        return cls(value=i, ty='int')
    def from_tint(cls, i, ty):
        return(cls(value=i, ty=ty))

@dataclass(frozen=True)
class StrLiteral(ArgExp):
    """ A string literal """
    value : str
    def __str__(self): # quote the string
        rv = f"\"{self.value}\""
        return rv

class C():
    """A block of c code that can optionally be used as a template """
    def __init__(self, base_str):
        self.base_str = (base_str)

    def find_identifiers(self):
        # Regex pattern to match $x or ${x}
        pattern = re.compile(r'\$\{?(\w+)\}?')
        # Find all matches
        matches = pattern.findall(self.base_str)
        return matches        
    def __call__(self, *args, **kwargs):
        identifiers = self.find_identifiers()
        # Map positional arguments to identifiers
        positional_args = {identifiers[i]: arg for i, arg in enumerate(args)}
        
        # Combine positional and keyword arguments
        all_args = {**positional_args, **kwargs}
        return Template(self.base_str).substitute(all_args)
    def __str__(self):
        return self.base_str
    
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
    state_ty : Ty
    arg_tys : list[Ty]
    ret_ty : Ty
    init_fn_name  : str
    fn_name : str
    fn : str
    init : str
    init_args : list[Val]
    __match_args__ = ("state_ty", "ret_ty", "fn_name", "arg_tys")


#### atom helpers
def has_state(atom : AtomDecl):
    return atom.state_ty is not None
def name(atom : AtomDecl):
    return atom.fn_name
def ret_ty(atom : AtomDecl):
    return atom.ret_ty


def var(name : str):
    return Var(name, None)

def argty(arg : ArgExp, ty : Ty):
    if type(arg) == Var:
        return Var(arg.name, ty)
    elif type(arg) == Val:
        return Val(arg.value, ty)
    elif (type(arg) == StrLiteral):
        return StrLiteral(arg.value)
    elif (type(arg) == CompoundVar):
        return replace(arg, ty=ty)
    else:
        print("type(arg): ", type(arg))
        print("arg: ", arg)
        raise("error: arg is not a var or val")

def is_pointer(ty : Ty):
    match ty:
        case Ptr(_):
            return True
        case _:
            return False

def ref(var : Var):
    # return a reference to a variable
    if is_pointer(var.ty):
        return f"{var.name}"
    else:
        return f"&{var.name}"


@dataclass(frozen=True)
class NicPort:
    """
    A NIC port is a pair of device and queue number.
    """
    dev : str = None    # a name for reference, not used in codegen
    devnum : int = None # dpdk's internal device number for the dev
    queue : int = None  # the queue number on the device
    __match_args__ = ("dev", "queue")
    def __str__(self):
        return f"{self.dev}(@queue={self.queue})"    

@dataclass(frozen=True)
class PipeBase:
    """
    A pipe is a function that can be applied to a packet. It is 
    built using combinators defined below.
    Every pipe has a start and end location.
    NOTE: to correctly initialize a subclass of PipeBase when 
          calling the autogenerated constructor, you MUST 
          pass the "start" and "end" arguments, OR 
          pass the subclass arguments as keyword arguments.
          Otherwise, the first two arguments that you expect 
          to go to the subclass fields will be used to 
          initialize start and end!
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
    args : Optional[list[ArgExp]] = None
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
class Switch(PipeBase):
    """
    A pipe that switches on a bound variable, executing the 
    pipe corresponding to the value of the variable.
    """
    var : Optional[Var] = None
    cases : Optional[dict[int, PipeBase]] = None
    __match_args__ = ("var", "cases")



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
class MainPipe(PipeBase):
    """deliver a packet from the network to one or more pipes based on some policy.
       The "policy" is just an uninterpreted string for now. """    
    cstr : list[C] = None
    policy  : Optional[str] = None
    pipes : Optional[list[(NicPort,PipeBase)]] = None
    __match_args__ = ("policy", "pipes")  
    def __str__(self):
        pipemap_strs = '\n  '.join([f"{str(nicport)} -> {pretty_print(pipe, indent=2)}"for (nicport, pipe) in self.pipes])
        return f"MainPipe(\n  {pipemap_strs}\n)"
    

#### builtin constants
start_loc = "network"
default_state_arg = Var("_no_state", "char *")
packet_arg = Var("pkt", "char *")


#### helpers
global idctr; idctr = -1
def fresh_name(name):
    global idctr
    idctr = idctr + 1
    return f"{name}_{idctr}"

def fresh_var(var : Var):
    return Var(name=fresh_name(var.name), ty=var.ty)


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
        case _: 
            print(type(pipe))
            print("pipe_to_string: case failure")
            exit(1)

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
