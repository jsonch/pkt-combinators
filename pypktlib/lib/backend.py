"""
    Compile a pipeline to a C-DPDK function. 
"""
import os, shutil
from itertools import chain
from .syntax import *
#### Pipeline IR

# the IR is a graph of named, located pipe segments.
# pipes have a new constructor, Move, that represents the effectful move from one segment to another.

@dataclass(frozen=True)
class Segment():
    """ A Named pipe segment """
    name      : str = None
    rx_queue  : str = None
    rx_dev    : tuple[str, int] = None # if you enter the segment from the network, this will be filled
    location  : str = None
    pipe      : PipeBase = None
    __match_args__ = ('name', 'rx_queue', 'location', 'pipe')
    def __str__(self):
        return f"segment {self.name} at {self.location}:{newtab(2)}{tablines(2, pretty_print(self.pipe))}"

@dataclass(frozen=True)
class Queue():
    """ A typed queue """
    name      : str = None
    src_seg   : str = None
    dst_seg   : str = None
    msg_ty    : list[Var] = None
    def __str__(self):
        return f"queue {self.name} ({self.src_seg} -> {self.dst_seg}) : {self.msg_ty}"
    

@dataclass(frozen=True)
class Move(PipeBase):
    """
        Move(dst_queue) is a new pipe in the IR that represents 
        an effectful move from one segment / location to another.
    """
    dst_queue: Queue = None
    __match_args__ = ('dst_queue',)
    def __str__(self):
        if self.dst_queue == None:
            return f"move<to software>)"
        return f"move<{self.dst_queue.msg_ty}>({self.dst_queue.name})"


@dataclass(frozen=True)
class IrProg():
    """ A program in the IR """
    cstr     : Optional[str] = None # custom c helpers used inside of atoms
    segments : list[Segment] = None
    queues   : list[Queue] = None
    def __str__(self):
        lines = []
        lines.append("segments:")
        for s in self.segments:
            lines.append(str(s))
        lines.append("queues:")
        for q in self.queues:
            lines.append(str(q))
        return "\n".join(lines)        

def fresh_segment_name(loc): 
    return fresh_name(str(loc)+"_segment")

def fresh_queue_name():
    return fresh_name("queue")

def pipe_to_segments(prog: IrProg, cur_seg_name : str, bound_vars : list[Var], pipe : PipeBase, rx_dev=None):
    """
    Convert a pipe program into a graph of segments connected by queues.
    """
    match pipe:
        case At(location, inner_pipe):
            # an "at" pipe starts a new segment.
            next_name = fresh_segment_name(location)
            if (rx_dev == None):
                # build the queue that connects the two segments
                next_queue = Queue(name=fresh_queue_name(), src_seg=cur_seg_name, dst_seg=next_name, msg_ty=bound_vars)
                # build the next segment
                prog, after_next_name, next_pipe = pipe_to_segments(prog, next_name, bound_vars, inner_pipe)
                next_segment = Segment(name=next_name, rx_queue=next_queue.name, location=location, pipe=next_pipe)
                # finally, return the move pipe, which is just an instruction to put the current environment
                # into a message for the queue and then return.
                rv = Move(dst_queue=next_queue)
                rv = replace(rv, start=pipe.start, end=pipe.end)
                return IrProg(prog.cstr, prog.segments+[next_segment], prog.queues+[next_queue]), after_next_name, rv
            else:
                # build the next segment
                prog, after_next_name, next_pipe = pipe_to_segments(prog, next_name, bound_vars, inner_pipe)
                next_segment = Segment(name=next_name, rx_queue=None, location=location, pipe=next_pipe)
                next_segment = replace(next_segment, rx_dev=rx_dev)
                rv = Move(dst_queue=None)
                return IrProg(prog.cstr, prog.segments+[next_segment], prog.queues), after_next_name, rv
        case Seq(left, right):
            prog, after_left_name, new_left = pipe_to_segments(prog, cur_seg_name, bound_vars, left)
            if (after_left_name != cur_seg_name):
                print("unexpected: sequence with a move in the left side")
                exit(1)
            prog, after_right_name, new_right = pipe_to_segments(prog, after_left_name, bound_vars, right)
            return prog, after_right_name, replace(pipe, left=new_left, right=new_right)
        case Let(ret, left, right):
            prog, after_left_name, new_left = pipe_to_segments(prog, cur_seg_name, bound_vars, left)
            if (after_left_name != cur_seg_name):
                print("unexpected: let with a move in the left side")
                exit(1)
            new_bound_vars = bound_vars + [ret]
            prog, after_right_name, new_right = pipe_to_segments(prog, after_left_name, new_bound_vars, right)
            return prog, after_right_name, replace(pipe, left=new_left, right=new_right)
        case Atom(_, _, _, _):
            # atoms don't change the segment or bind variables. No change. 
            return prog, cur_seg_name, pipe    
        case Exit(_):
            # exit pipes don't change the segment or bind variables. No change.
            return prog, cur_seg_name, pipe
        case Noop():
            print("error: got a noop in pipe_to_segment.")
            exit(1)
        case Switch(_, cases):
            # switch pipes don't change the segment or bind variables.
            # Also, the switch is inlined, so we don't need to know the names of the segments that 
            # it jumps to, or the variables that are bound in each case. 
            # but, we do have to recurse on the cases, to segment those pipes.
            new_cases = {}
            for k, v in cases.items():
                prog, _, new_case = pipe_to_segments(prog, cur_seg_name, bound_vars, v)
                new_cases[k] = new_case
            return prog, cur_seg_name, replace(pipe, cases=new_cases)
        case MainPipe(_, pipes):
            new_pipes = []
            for k, v in pipes:
                prog, _, new_pipe = pipe_to_segments(prog, cur_seg_name, bound_vars, v, rx_dev=k)
                new_pipes.append((k, new_pipe))
            return prog, cur_seg_name, replace(pipe, pipes=new_pipes)
        case MoveFrontend(_):
            print("unexpected: a move from the frontend. Should be converted to At.")
            exit()
        
            
def pipe_to_ir(pipe : PipeBase):
    # the main pipe is a special case, because it doesn't have a name or location
    # and it doesn't have a return variable. 
    # we'll give it a fresh name and location, and a return variable, 
    # and then run the normal pipe_to_segments on it.
    main_segment_name = fresh_segment_name(start_loc)
    # note: the packet argument is a global variable, hence why we pass it to bound vars
    prog, _, main_segment_pipe = pipe_to_segments(IrProg(None, [], []), main_segment_name, [packet_arg], pipe)
    main_segment = Segment(name=main_segment_name, location=start_loc, pipe=main_segment_pipe)
    segments = prog.segments + [main_segment]
    segments = segments[::-1]
    prog = IrProg(pipe.cstr, segments, prog.queues)
    return prog


        

def backend_passes(pipe : PipeBase):
    # 6. convert to segment-graph IR
    rv = pipe_to_ir(pipe)
    return rv


#### DPDK code generation


