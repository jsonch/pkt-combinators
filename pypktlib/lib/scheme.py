from .stdlib import *
from .syntax import *
from .frontend import *


class Scheme():
    """The base class for a parallelization scheme"""
    def __init__(self):
        pass

    def transform(self, pipe : PipeBase):
        return pipe
    
def MoveAndSeq(loc, pipe):
    return Seq(left=MoveFrontend(location=loc), right=pipe)

def arg_vars_in_pipe(pipe):
    """Set of variable names used in the pipe"""
    vars = set()
    rec = lambda p : vars.update(arg_vars_in_pipe(p))
    match pipe:
        case Atom(state, _, args, return_var):
            for arg in args:
                if arg.base_name() != None:
                    vars.add(arg.base_name())
        case _:
            recurse(rec, pipe)
    return vars
    

def state_vars_in_pipe(pipe):
    """Set of variable names used in the pipe"""
    vars = set()
    rec = lambda p : vars.update(state_vars_in_pipe(p))
    match pipe:
        case Atom(state, _, args, return_var):
            if state:
                vars.add(state.name)
        case _:
            recurse(rec, pipe)
    return vars

def letswitch_vars_in_pipe(pipe):
    """Set of variable names used in the pipe"""
    vars = set()
    rec = lambda p : vars.update(letswitch_vars_in_pipe(p))
    match pipe:
        case Atom(state, _, args, return_var):
            if return_var:
                vars.add(return_var.name)
        case Let(var, left, right):
            vars.add(var.name)
            recurse(rec, pipe)
        case Switch(var, cases):
            vars.add(var.name)
            recurse(rec, pipe)
        case _:
            recurse(rec, pipe)
    return vars
def substitute_var(old_var, new_var, pipe):
    """This will replace uses of old_var with new_var. It won't replace binding occurrences or state variables."""
    rec = lambda p : substitute_var(old_var, new_var, p)
    vip = letswitch_vars_in_pipe(pipe) | arg_vars_in_pipe(pipe) | state_vars_in_pipe(pipe)
    print("old", old_var)
    print("vips", vip)
    print("pipe", pipe_to_string(pipe))
    #assert(old_var.name in vip)
    assert(not(new_var in vip))
    match pipe:
        case Atom(_, _, args, _):
            new_args = [v if v.name != old_var else new_var for v in args]
            return replace(pipe, args=new_args)
        case _: 
            return recurse(rec, pipe)

class FullParallel(Scheme):
    def __init__(self, compute_locations, shard_location=None):
        self.compute_locations = compute_locations
        self.shard_location = shard_location
        self.name = "FullParallel"
    #Split this one pipe into a shard location and a bunch of parallel comput cores, all running that pipe. 
    #Assumes total parallel, no shared state
    def transform(self, pipe : PipeBase):
        #create the shard atom
        decision_name = Var(name="auto_shard_decision_name")
        decider_atom_decl = rr_counter(len(self.compute_locations)).to_ir()
        shard_decider = Atom(atom=decider_atom_decl, args=[])
        cases = {}
        for s in self.compute_locations:
            cases[s] = MoveAndSeq(loc=str(s), pipe=freshen_all_vars(pipe))
        let_pipe = Let(ret=decision_name, left=shard_decider, right=Switch(var=decision_name, cases=cases))
        if self.shard_location != None:
            final_pipe = MoveAndSeq(str(self.shard_location), let_pipe)
        else:
            final_pipe = let_pipe
        return final_pipe

class ParallelShared(Scheme):
        def __init__(self, compute_locations, shard_location=None):
            self.compute_locations = compute_locations
            self.shard_location = shard_location
            self.name = "ParallelShared"

        def transform(self, pipe : PipeBase):
            print("called par shared transform")
            decision_name = Var(name="auto_shard_decision_name")
            decider_atom_decl = rr_counter(len(self.compute_locations)).to_ir()
            shard_decider = Atom(atom=decider_atom_decl, args=[])
            cases = {}
            instantiated_pipe = instantiate(pipe)
            for s in self.compute_locations:
                cases[s] = MoveAndSeq(loc=str(s), pipe=instantiated_pipe)
            let_pipe = Let(ret=decision_name, left=shard_decider, right=Switch(var=decision_name, cases=cases))
            if self.shard_location != None:
                final_pipe = MoveAndSeq(str(self.shard_location), let_pipe)
            else: 
                final_pipe = let_pipe
            return final_pipe

class SCRDeannotate(Scheme):
    """Annotator for SCR scheme. YOU SHOULD USE ONE SEPARATE OBJECT PER VARIABLE!"""
    def __init__(self):
        self.called = False
        self.name = "SRCDeannotate"

    def transform(self, pipe : PipeBase):
        """This will run the given pipe a number of times equal to the hist_length of this object and
            and substitute the history at that index for the value of the var. It will run the oldest 
            part of the history first up to the current packet last."""
        p = Noop()
        for i in range(self.hist_length-1, 0, -1):
            var_used = self.hist_var_names[i-1] if i!=0 else self.var
            p = Seq(left=substitute_var(self.var, var_used, pipe), right=p)
        return p
    def annotator(self, var, hist_length):
        return self._SCRAnnotate(var, hist_length, self)
    class _SCRAnnotate(Scheme):
        """Inner class for deannotator, get one by calling annotator on the outer object!"""
        def __init__(self, var, hist_length, deannotate_instance):
            """Returns a pipe that annotates a packet with metadata history of the given length
                EFFECT: Sets the var_name and hist_length for this object to deannotate when it transforms a pipe
            """
            if hist_length < 2:
                raise "Hist length should be more than one packet."
            self.name = "SCRAnnotate"
            self.var = var
            deannotate_instance.var = self.var
            self.hist_length = hist_length
            deannotate_instance.hist_length = self.hist_length
            self.hist_var_names = [Var(name=f"{var}_hist_{i}", ty=var.ty) for i in range(hist_length)]
            deannotate_instance.hist_var_names = self.hist_var_names
        
        def Transform(self, pipe):
            print("running annotate transform on pipe", pipe_to_string(pipe))
            p = pipe
            for i in range(self.hist_length-1, 0, -1):
                    p = Let(ret = self.hist_var_names[i], left=(self.hist_var_names[i-1] if i!=0 else self.var), right=p)
            return p



