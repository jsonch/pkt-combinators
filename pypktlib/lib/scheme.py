from .stdlib import *
from .syntax import *
from .frontend import *


class Scheme():
    """The base class for a parallelization scheme"""
    def __init__(self):
        pass

    def transform(self, pipe : PipeBase):
        return pipe
    

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
            cases[s] = At(location=s, inner_pipe=pipe)
        let_pipe = Let(ret=decision_name, left=shard_decider, right=Switch(var=decision_name, cases=cases))
        if self.shard_location != None:
            final_pipe = At(location=self.shard_location, inner_pipe=let_pipe)
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
                cases[s] = At(location=s, inner_pipe=instantiated_pipe)
            let_pipe = Let(ret=decision_name, left=shard_decider, right=Switch(var=decision_name, cases=cases))
            if self.shard_location != None:
                final_pipe = At(location=self.shard_location, inner_pipe=let_pipe)
            else: 
                final_pipe = let_pipe
            return final_pipe