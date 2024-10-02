"""Frontend syntax for Mario."""
from typing import TypeVar, Generic, get_args, Optional
from copy import copy
from . import syntax

C = syntax.C # uninterpreted C code

# types
class UserTy():
    """A named user-defined type"""
    def __init__(self, name, cstr=None):
        self.name = name
        self.cstr = cstr
    def to_ir(self):
        return syntax.TyName(name=self.name, cstr = self.cstr)

# pointer type. Maybe not necessary?
T = TypeVar('T')
class Ptr(Generic[T]):
    def __init__(self, type_arg=None):
        self._type_arg = type_arg
    @classmethod
    def __class_getitem__(cls, item):
        return cls(type_arg=item)
    def to_ir(self):
        type_var = self._type_arg
        ir_type_var = type_var.to_ir()
        return syntax.Ptr(ir_type_var)
ref = Ptr


# generic metaclass generator for a class Foo with a 
# constructor that takes a single string argument 
# and wants Foo.xyz to evaluate to Foo("xyz")
def DotMakerMeta(str_to_obj):
    class DotMaker(type):
        def __getattr__(self, attr):
            return str_to_obj(attr)
    return DotMaker

class Meta(metaclass = DotMakerMeta(lambda name : Meta(name))):
    """A pipe variable"""
    def __init__(self, s):
        self.s = s
    def __lt__(self, other):
        # complete a let
        other.name = self
        return other
    def __str__(self):
        return self.s
    def to_ir(self):
        return syntax.var(self.s)

class Bank(metaclass = DotMakerMeta(lambda name : Bank(name))):
    """A state instance label"""
    next_anon = 0
    def __init__(self, name=None):
        Bank.next_anon += 1
        if name == None:
            name = "anon_state_"+str(Bank.next_anon)
        self.name = name
        self.instance_num = 1
    @classmethod
    def new(cls, name, instance_num):
        rv = cls(name)
        rv.instance_num = instance_num
        return rv
    def __str__(self):
        return f"{self.name}_instance_{self.instance_num}"
    def to_ir(self, ty):
        return syntax.Var(str(self), ty)

class LocMaker(type):
    def __getitem__(cls, key):
        return key
class Core(metaclass=DotMakerMeta(lambda name : Core(name))):
    def __init__(self, name=None):
        self.name = name
    def to_ir(self):
        return self.name



class AtomState():
    """A global state object to use in atoms"""
    def __init__(self, ty, init=None, cstr=None, initargs = None):
        self.ty = ty     # type of the state in mario
        self.init = init # name of initializer function
        self.cstr = cstr # implementation in c
        self.initargs = initargs
    def __call__(self, *args):
        """bind the initialization arguments for the constructor"""
        return AtomState(self.ty, self.init, self.cstr, args)
    def to_ir(self):
        """Return a named type in the ir"""
        return self.ty.to_ir()


S = TypeVar('S') # State type
A = TypeVar('A') # Arg types
R = TypeVar('R') # Return type

class Atom(Generic[S, A, R]):
    # when converting into a concrete class, 
    # store the concrete types.
    concrete_types = None
    @classmethod
    def __class_getitem__(cls, params):
        cls.concrete_types = params # (S, A, R)
        return super().__class_getitem__(params)

    def __init__(self, 
        name = "some_f",
        f = "// c function : void* -> char* -> A -> R -> () "):
        # get state type from annotations
        self.fname = name
        self.f = f
        state_type, _, _ = self.concrete_types
        if (state_type != None):
            self.init = state_type.cstr
            self.initname = state_type.init
            self.init_args = state_type.initargs
        else:
            self.init = None
            self.initname = None
            self.init_args = []
        self.instance_id = None # needed by AtomCall, held here for syntax convenience
    def typeargvals(self):
        """translate type arguments into values"""
        orig_class = getattr(self, '__orig_class__', None)
        state_type, arg_types, return_type = get_args(orig_class)
        ir_state_type = None
        if state_type != type(None):
            ir_state_type = state_type.to_ir()
        ir_arg_types = []
        if type(arg_types) == list:
            for arg in arg_types:
                ir_arg_types.append(arg.to_ir())
        elif arg_types != type(None): 
            ir_arg_types.append(arg_types.to_ir())
        ir_return_type = None
        if return_type != type(None):
            ir_return_type = return_type.to_ir()
        return ir_state_type, ir_arg_types, ir_return_type
        
        Ty.to_ir_list(arg_types), Ty.to_ir(return_type)
        # return Ty.to_ir(state_type), Ty.to_ir_list(arg_types), Ty.to_ir(return_type)

    def to_ir(self):
        # translate the base atom to the IR. 
        # does not include the instance id
        orig_class = getattr(self, '__orig_class__', None)
        state_type, arg_types, return_type = self.typeargvals()

        rv = syntax.AtomDecl(
            state_ty=state_type, 
            arg_tys=arg_types, 
            ret_ty=return_type, 
            init_fn_name=self.initname, 
            fn_name=self.fname, 
            fn=self.f, 
            init=self.init, 
            init_args=[syntax.Val.from_int(a) for a in self.init_args])
        return rv
    def __call__(self, *args):
        # get type information from Atom class parameters
        orig_class = getattr(self, '__orig_class__', None)
        if (self.instance_id == None):
            self.instance_id = Bank()
        if orig_class:
            state_type, arg_types, return_type = get_args(orig_class)
            if isinstance(arg_types, tuple):
                arg_types = list(arg_types)
            else:
                arg_types = [arg_types]
            _args = []
            for a in args:
                if (type(a) == int):
                    _args.append(syntax.Val.from_int(a))
                elif(type(a) == Meta):
                    _args.append(syntax.Var(name=str(a)))
                elif(type(a) == str):
                    _args.append(syntax.StrLiteral(a))
            # args = [str(a) for a in args]
            rv = AtomCall(self, self.fname, arg_types, return_type, _args, self.instance_id)
            return rv
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

updated_pipes = {}

class PipeBase():
    """ The base pipe class. Defines all the 
        overloads for syntactic sugar. """
    instance_num = 1 # incremented on each "New" or "Pipe"    
    def __init__(self):
        pass
    def __rshift__(fst, snd):
        """Sequence: x >> y"""
        dprint(f"__rshift__({str(fst)}, {str(snd)})")
        return Seq(fst, snd)
    def __gt__(let, exp_before_let):
        global updated_pipes
        # if exp_before_let in updated_pipes:
        #     exp_before_let = updated_pipes[exp_before_let]
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
        dprint("exp_before_let: >>>>\n" + str(exp_before_let) + "\n<<<<")
        dprint("let: >>>>\n" + str(let) + "\n<<<<")
        if (type(exp_before_let) == Seq):
            return(Seq(exp_before_let.fst, PipeLet(exp_before_let.snd, let.inner, let.outer)))
        elif (type(exp_before_let) == PipeLet):
            print ("=====exp_before_let.outer=====")   
            print (exp_before_let.outer)
            print("-----------------")  
            updated_pipe = PipeLet(exp_before_let.name, exp_before_let.inner, PipeLet(exp_before_let.outer, let.inner, let.outer))
            updated_pipes[let] = updated_pipe
            dprint("updated_pipe: >>>>\n" + str(updated_pipe) + "\n<<<<")
            return(updated_pipe)
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
        return PipeLet(None, self, other)
    def __mod__(self, other): 
        """ "in" token for let binding:  
            Vars.x <- foo() %
            ==> 
            let x = foo() in
        """
        return PipeLet(None, self, other)
    def __matmul__(pipe, loc):
        """
            Pipe locator:
            (foo() @ Core[1]) 
            ==>
            at Core[1] do foo()
        """
        return At(loc, pipe)
    def to_ir(self):
        print("compiler error: to_ir not implemented")
        print("type: "+str(type(self)))
        exit(1)


class Pipe(PipeBase):
    """Toplevel pipe. Any Pipe that a user defines 
       should be wrapped in this. e.g.: 
       Pipe(foo() @ Core[1])
    """
    def __init__(self, p:PipeBase):
        # dprint("Pipe.__init__")
        # dprint("p="+str(p))
        PipeBase.instance_num += 1
        self.p = refresh(p, PipeBase.instance_num)
    def __str__(self):
        return f"Pipe(\n{indent(2, str(self.p))}\n)"
    def __repr__(self):
        return f"Pipe({str(self.p)})"
    def to_ir(self):
        return self.p.to_ir()

class PipeLet(PipeBase):
    """Call an atom and bind its output to a variable"""
    def __init__(self, name, inner, outer):
        self.name=name
        self.inner=inner
        self.outer=outer
    def __str__(self):
        return f"let {self.name} = {str(self.inner)} in\n{str(self.outer)}"
    def __repr__(self):
        return f"Let({self.name}, {repr(self.inner)}, {repr(self.outer)})"
    def to_ir(self):
        ret = syntax.Var(name=str(self.name))
        left = self.inner.to_ir()
        right = self.outer.to_ir()
        rv = syntax.Let(ret=ret, left=left, right=right)
        return rv

class Seq(PipeBase):
    """A sequence of two pipes. The first pipe is run, 
        its output (besides the implicit packet) is discarded, 
        and the second pipe is run."""
    def __init__(self, fst, snd):
        self.fst = fst
        self.snd = snd
    def __str__(self):
        if len(str(self.fst)) > 50:
            return f"{str(self.fst)}\n>> {str(self.snd)}"
        else:
            return f"{str(self.fst)} >> {str(self.snd)}"
    def __repr__(self):
        return f"{repr(self.fst)} >> {repr(self.snd)}"
    def to_ir(self):
        fst = self.fst.to_ir()
        snd = self.snd.to_ir()
        return syntax.Seq(left=fst, right=snd)

class AtomCall(PipeBase):
    """Call an atom with arguments"""
    def __init__(self, atom, name, argtys, retty, args, instance):
        self.atom = atom
        self.name = name
        self.argtys = argtys
        self.retty = retty
        self.args = args
        self.instance_id : Optional[Bank] = instance
    def __str__(self):
        argtys_str = ", ".join([str(t) for t in self.argtys])
        if len(self.argtys) == 0:
            argtys_str = "()"
        args_str = ", ".join([str(a) for a in self.args])
        if (self.instance_id == 0):
            return f"{self.name}({args_str})"
        else:
            return f"{self.name}[{self.instance_id}]({args_str})"
    def to_ir(self):
        atom = self.atom.to_ir() 
        args = self.args # args are already in IR
        # if (args != None):
        #     args = [a.to_ir() for a in args]
        # else:
        #     print("compiler error: args is None")
        #     exit(1)
        state = None
        if (atom.state_ty != None):
            state = self.instance_id.to_ir(atom.state_ty)
        return syntax.Atom(state=state, atom=atom, args=args)
    def __repr__(self):
        argtys_str = ", ".join([str(t) for t in self.argtys])
        return f"AtomCall({self.name} : ({argtys_str} -> {str(self.retty)}) args = [{', '.join(map(str, self.args))}])"

class SwitchPipe(PipeBase):
    """match on a pipe variable to select a pipe """
    def __init__(self, matchvar, cases):
        self.matchvar = matchvar
        self.cases = cases
    def to_ir(self):
        matchvar = self.matchvar.to_ir()
        cases = {k: v.to_ir() for k, v in self.cases.items()}
        return syntax.Switch(var=matchvar, cases=cases)
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
    def __str__(self):
        return f"At({str(self.loc)}, {str(self.pipe)})"
    def __repr__(self):
        return f"At({str(self.loc)}, {repr(self.pipe)})"
    def to_ir(self):
        return syntax.At(location=Core.to_ir(self.loc), inner_pipe=self.pipe.to_ir())

def refresh(p : PipeBase, instance_num):
    # refresh the instance id or 
    # recurse on children
    # new_p = copy(p)
    if isinstance(p, AtomCall):
        new_p = copy(p)
        new_p.instance_id = Bank.new(new_p.instance_id.name, instance_num)
        return new_p
    else:
        new_p = copy(p)
        items = new_p.__dict__.items()
        new_items = {}
        for (k, v) in items:
            if (isinstance(v, PipeBase)):
                new_items[k] = refresh(v, instance_num)
            else:
                new_items[k] = v
        new_p.__dict__ = new_items
        return new_p

class New(PipeBase):
    """refresh all the state names in a pipe"""
    def __init__(self, pipe):
        PipeBase.instance_num += 1
        self.pipe = refresh(pipe, New.instance_num)
    def __str__(self):
        return f"New({str(self.pipe)})"

    def __repr__(self):
        return f"New({repr(self.pipe)})"
    
    def to_ir(self):
        # noop because the inner pipe is already refreshed upon init
        return self.pipe.to_ir()


# input: 
# toplevel program is a map of device to pipe
# i.e., {(eth_dev_number, queue_number) : pipe}
# Note that Main is not a pipe itself
class Main():
    def __init__(self, includes : list[C] = [], pipes : dict[(int, int), PipeBase] = {}):
        self.cstr = includes
        self.mapping = pipes
    def __str__(self):
        ret_str = "Main({\n"
        for (k, v) in self.mapping.items():
            ret_str += f"{k}: {str(v)}\n"
        ret_str += "})"
        return ret_str
    def to_ir(self):
        nicport_pipes = []
        for ((dev_num, queue_num), pipe) in self.mapping.items():
            nicport = syntax.NicPort("", dev_num, queue_num)
            nicport_pipes.append((nicport, pipe.to_ir()))
        return syntax.MainPipe(cstr=self.cstr, policy=None, pipes=nicport_pipes)


class Exit(PipeBase):
    """Stop processing, optionally sending the packet to a port."""
    # TODO: split sending and stopping into two separate operations
    def __init__(self, out_port_tuple : Optional[tuple[int, int]]):
        self.out_port_tuple = out_port_tuple
    def __str__(self):
        if self.out_port_tuple:
            return f"Exit({self.out_port_tuple})"
        else:
            return "Exit()"
    def to_ir(self):
        nicPort = syntax.NicPort("", self.out_port_tuple[0], self.out_port_tuple[1])
        return syntax.Exit(dest=nicPort)
    

# class Send(PipeBase):
#     """Send the current packet to a port id.
#        This does not stop processing."""    
#     def __init__(self, port):
#         self.port = port
#     def __str__(self):
#         return f"Send({str(self.port)})"

# class Stop(PipeBase):
#     """Stop processing the current packet"""
#     def __str__(self):
#         return "Stop()"

def Switch(var):
    """ a little sugar for switch """
    def inner(cases):
        return SwitchPipe(var, cases)
    return inner

default = "_"

def Send(port):
    return Exit((port, 0))

### misc helpers
def dprint(s):
    return
    print(s)

def indent(n, s):
    """Indent a string by n spaces and add 
       n spaces before each newline in it"""
    lines = s.split("\n")
    indented_lines = [n * " " + line for line in lines]
    return "\n".join(indented_lines)
