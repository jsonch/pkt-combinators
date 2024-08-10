"""Frontend syntax for Mario"""
from typing import TypeVar, Generic, get_args
from util import *
from names import *
from copy import copy

class Var:
    """A pipe variable"""
    def __init__(self, s):
        self.s = s
    def __lt__(self, other):
        # complete a let
        other.name = self
        return other
    def __str__(self):
        return self.s

class VarMaker(type):
    """Makes variables from attribute references """
    def __getattr__(cls, attr):
        return Var(attr)

class Meta(metaclass = VarMaker):
    """Lets you write string ids 
       without quotes. Meta.x evals to "x" """
    def __init__(self):
        pass

class StateMaker(type):
    """Makes a state name"""
    def __getattr__(cls, attr):
        return attr

class State(metaclass = StateMaker):
    next_iid = 0
    def __init__(self, name, id):
        self.name = name
        self.id = id
    @classmethod
    def new(cls):
        cls.next_iid += 1
        return cls("state", cls.next_iid)
    def refresh(self):
        # LEFT OFF HERE. fiddling with refresh.
        self.name = "refreshed_"+self.name
        self.id = self.id + 1
    def __str__(self):
        return f"{self.name}_{self.id}"


S = TypeVar('S') # State type
A = TypeVar('A') # Arg types
R = TypeVar('R') # Return type
class Atom(Generic[S, A, R]):
    def __init__(self,
        name = "some_f",
        f = "// c function : void* -> char* -> A -> R -> () ",
        init = None,
        init_args = None):
        self.fname = name
        self.f = f
        self.init = init
        self.init_args = init_args
        self.instance_id = None
    def __call__(self, *args):
        # get type information from Atom class parameters
        orig_class = getattr(self, '__orig_class__', None)
        if (self.instance_id == None):
            self.instance_id = State.new()
        if orig_class:
            state_type, arg_types, return_type = get_args(orig_class)
            if isinstance(arg_types, tuple):
                arg_types = list(arg_types)
            else:
                arg_types = [arg_types]
            return AtomCall(self.fname, arg_types, return_type, args, self.instance_id)
        else:
            print("compiler error -- could not retrieve atom class in atom instance?")
            raise TypeError
            exit(1)
    def __getitem__(self, instance_id):
        # select which instance of the Atom you are using. 
        # by default, use the first instance. 
        updated_atom = copy(self)
        updated_atom.instance_id = instance_id
        return updated_atom

    def __str__(self):
        if ("args" in self.__dict__):
            args_str = ", ".join(map(str, self.args))
            return f'{self.fname}({args_str})'
        else:
            return self.fname


class PipeBase():
    """ The base pipe class. Defines all the 
        overloads for syntactic sugar. """
    def __init__(self):
        self.children = []
    def __rshift__(fst, snd):
        """Sequence: x >> y"""
        dprint(f"__rshift__({str(fst)}, {str(snd)})")
        return Seq(fst, snd)
    def __gt__(let, exp_before_let):
        # correct p >> x <- foo % bar -- which parses as: 
        # (p >> x) < (foo % bar)
        # but we want: 
        # p >> (x < foo % bar)
        # and correct x <- foo % y <- bar % baz
        # which parses as:
        # (x <- (foo % y)) <- (bar % baz)
        # but we want: 
        # (x <- foo % (y <- bar % baz))
        dprint("__gt__")
        dprint("exp_before_let: " + str(exp_before_let))
        dprint("let: " + str(let))
        if (type(exp_before_let) == Seq):
            return(Seq(exp_before_let.fst, Let(exp_before_let.snd, let.inner, let.outer)))
        elif (type(exp_before_let) == Let):
            return(Let(exp_before_let.name, exp_before_let.inner, Let(exp_before_let.outer, let.inner, let.outer)))
        else:
            print("neither a let nor a seq comes before the let. Impossible?")
            exit(1)
    def __neg__(self):
        """noop, so that you can write assignment as x <- foo """
        dprint("__neg__")
        dprint("self: "+str(self))
        return self
    def __div__(self, other): 
        """ "in" token for let binding:  
            Vars.x <- foo() \
            ==> 
            let x = foo() in
        """
        return Let(None, self, other)
    def __mod__(self, other): 
        """ "in" token for let binding:  
            Vars.x <- foo() %
            ==> 
            let x = foo() in
        """
        return Let(None, self, other)
    def __matmul__(pipe, loc):
        """
            Pipe locator:
            (foo() @ Core[1]) 
            ==>
            at Core[1] do foo()
        """
        return At(pipe, loc)


class Pipe(PipeBase):
    """Toplevel pipe. Any Pipe that a user defines 
       should be wrapped in this. e.g.: 
       Pipe(foo() @ Core[1])
    """
    def __init__(self, p:PipeBase):
        # dprint("Pipe.__init__")
        # dprint("p="+str(p))
        self.p = p
        self.children = [p]
    def __str__(self):
        return f"Pipe(\n{indent(2, str(self.p))}\n)"
    def __repr__(self):
        return f"Pipe({str(self.p)})"
    
class Let(PipeBase):
    """Call an atom and bind its output to a variable"""
    def __init__(self, name, inner, outer):
        self.name=name
        self.inner=inner
        self.outer=outer
        self.children = [inner, outer]
    def __str__(self):
        return f"let {self.name} = {str(self.inner)} in\n{str(self.outer)}"
    def __repr__(self):
        return f"Let({self.name}, {repr(self.inner)}, {repr(self.outer)})"
class Seq(PipeBase):
    """A sequence of two pipes. The first pipe is run, 
        its output (besides the implicit packet) is discarded, 
        and the second pipe is run."""
    def __init__(self, fst, snd):
        self.fst = fst
        self.snd = snd
        self.children = [fst, snd]
    def __str__(self):
        if len(str(self.fst)) > 50:
            return f"{str(self.fst)}\n>> {str(self.snd)}"
        else:
            return f"{str(self.fst)} >> {str(self.snd)}"
    def __repr__(self):
        return f"{repr(self.fst)} >> {repr(self.snd)}"
class AtomCall(PipeBase):
    """Call an atom with arguments"""
    def __init__(self, name, argtys, retty, args, instance):
        self.name = name
        self.argtys = argtys
        self.retty = retty
        self.args = args
        self.instance_id = instance
    def __str__(self):
        argtys_str = ", ".join([str(t) for t in self.argtys])
        if len(self.argtys) == 0:
            argtys_str = "()"
        args_str = ", ".join([str(a) for a in self.args])
        if (self.instance_id == 0):
            return f"{self.name}({args_str})"
        else:
            return f"{self.name}[{self.instance_id}]({args_str})"

        # return f"{self.name}: ({argtys_str} -> {self.retty})({args_str})"
    def __repr__(self):
        argtys_str = ", ".join([str(t) for t in self.argtys])
        return f"AtomCall({self.name} : ({argtys_str} -> {str(self.retty)}) args = [{', '.join(map(str, self.args))}])"
class SwitchPipe(PipeBase):
    """match on a pipe variable to select a pipe """
    def __init__(self, matchvar, cases):
        self.matchvar = matchvar
        self.cases = cases
        self.children = list(cases.values())
    def __str__(self):
        cases_str = "{\n"
        for val, pipe in self.cases.items():
            if ("\n" in str(pipe)):
                cases_str += f"{val}: "
                cases_str += "(\n"
                cases_str += indent(2, str(pipe))
                cases_str += "\n)"
                # add a comma if this is not the last case
                if val != list(self.cases.keys())[-1]:
                    cases_str += ",\n"
                else:
                    cases_str += "\n"
            else:
                cases_str += f"  {val}: "
                cases_str += str(pipe) 
                if val != list(self.cases.keys())[-1]:
                    cases_str += ",\n"
                else:
                    cases_str += "\n"
        cases_str += "}"
        cases_str = indent(2, cases_str)
        return f"Switch({self.matchvar}, {cases_str})"
    def __repr__(self):
        cases_str = "{\n"
        for val, pipe in self.cases.items():
            cases_str += f"{val}:{repr(pipe)}\n"
        cases_str += "}"
        return f"Switch({self.matchvar}, {cases_str})"

class At(PipeBase):
    """locate a pipe's computation"""
    def __init__(self, loc, pipe):
        self.loc=loc
        self.pipe = pipe
        self.children = [pipe]
    def __str__(self):
        return f"At({str(self.loc)}, {str(self.pipe)})"
    def __repr__(self):
        return f"At({str(self.loc)}, {repr(self.pipe)})"

def refresh(p : PipeBase):
    # refresh the instance id or 
    # recurse on children
    new_p = copy(p)
    if isinstance(p, AtomCall):
        new_p.instance_id.refresh()     
    elif ("children" in p.__dict__):
        print(type(p))
        for pchild in new_p.children:
            refresh(pchild)
    return new_p        

class New(PipeBase):
    """refresh all the state names in a pipe"""
    def __init__(self, pipe):
        self.pipe = refresh(pipe)
        self.children = [self.pipe]
    def __str__(self):
        return f"New({str(self.pipe)})"

    def __repr__(self):
        return f"New({repr(self.pipe)})"

class Receive(PipeBase):
    """Fetch a packet from a port id"""
    def __init__(self, port):
        self.port = port
    def __str__(self):
        return f"Receive({str(self.port)})"
    
class Send(PipeBase):
    """Send the current packet to a port id.
       This does not stop processing."""
    def __init__(self, port):
        self.port = port
    def __str__(self):
        return f"Send({str(self.port)})"

class Stop(PipeBase):
    """Stop processing the current packet"""
    def __str__(self):
        return "Stop()"

def Switch(var):
    """ a little sugar for switch """
    def inner(cases):
        return SwitchPipe(var, cases)
    return inner

default = "_"
