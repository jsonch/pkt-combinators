#This takes in a pipeline, as an AST from syntax.py, and outputs a new one with the optimization. 
#It will use one core (The first one?) to do the history appending and 
from .backend import *
from .syntax import *
#Give me a pre- and post-annotation segment, and the location of the annotator. Return a pipe with the annotation optimization installed
def transform(pre_p : Segment, post_p : Segment, l : str):
    #Assume that pre_p is one segment that happens entirely at location l

    assert pre_p.location == l, "location is " + pre_p.location

    #We will find all of the queues that take in arguments from this segment. Their list of variables are what we are going to annotate with.
    #TODO: For now just does one. I don't know what to do if the list of vars is different for each one. 
    queue_subsequent = next(filter(lambda q: q.src_seg == pre_p.name, post_p.queues))


    print(pretty_print(post_p))
    print(generate_history_initialization_function(queue_subsequent.msg_ty, 2))
    print(generate_c_function_annotator(queue_subsequent.msg_ty, 2))
    print("NEW PIPE")
    new_seg = annotate(pre_p, queue_subsequent.msg_ty)
    #Put the new segment in place of the old, adding the annotator.
    other_segs = list(filter(lambda s: s.name != pre_p.name, post_p.segments))
    other_segs.append(new_seg)
    new_prog = replace(post_p, segments=other_segs)
    print(pretty_print(new_prog))


def annotate(pre_p : Segment, vars : list[Var]):
    #get the underlying pipe. Have already checked location.
    under_p = pre_p.pipe
    new_p = seq(under_p, do(annotator_atom(vars), [v.name for v in vars], state="history_var_name"))
    print("Printed new segment!")
    return replace(pre_p, pipe=new_p)

def deannotate(post_p : PipeBase, vars : list[Var]):
    return None

def annotator_atom(vars : list[Var]):
    return atom("void*", "initialize_history", "void*", "my_function", [v.ty for v in vars])

def deannotator_atom(vars : list[str]):
    return None

def generate_c_function_annotator(vars: list[Var], k: int) -> str:
    # Generate the argument list for the function
    varlen = len(vars)
    args = []
    for var in vars:
        if var.name is not None and var.ty is not None:
            args.append(f"{var.ty} {var.name}")
    
    args_str = ", ".join(args)
    
    # Define the function signature
    func_signature = f"void my_function(void *state, {args_str})"
    
    # Generate the body of the function
    body = []
    
    # Add history logic
    body.append(f"    // Create a pointer to the history array with capacity for {k} calls")
    body.append(f"    {', '.join(var.ty for var in vars if var.ty is not None)} (*history)[{k}][{len(vars)}] = state;")
    body.append("    static int current_size = 0;")
    
    # Shift history to the right
    body.append("    // Shift the history to the right")
    body.append("    for (int i = current_size; i > 0; --i) {")
    body.append(f"        for (int j = 0; j < {varlen}; ++j) {{")
    body.append("            history[i][j] = history[i-1][j];")
    body.append("        }")
    body.append("    }")
    
    # Save the current call's arguments to the front of the history
    body.append("    // Save the current call's arguments to the front of the history")
    for i, var in enumerate(vars):
        if var.name is not None:
            body.append(f"    history[0][{i}] = {var.name};")
    
    # Update the current size
    body.append("    if (current_size < {k}) {")
    body.append("        ++current_size;")
    body.append("    }")
    
    # Combine everything into the final function string
    func_body = "\n".join(body)
    function = f"{func_signature} {{\n{func_body}\n}}"
    
    return function

def generate_history_initialization_function(vars: list[Var], k: int) -> str:
    # Generate the sizes for malloc
    sizes = [f"sizeof({var.ty})" for var in vars if var.ty is not None]
    total_size = " + ".join(sizes)
    total_size = f"({total_size}) * {k}"
    
    # Define the function signature
    func_signature = "void* initialize_history()"
    
    # Generate the body of the function
    body = []
    
    # Allocate memory for the history
    body.append(f"    // Allocate memory for the history of the last {k} calls")
    body.append(f"    void* history = malloc({total_size});")
    body.append("    if (history == NULL) {")
    body.append("        // Handle malloc failure")
    body.append("        return NULL;")
    body.append("    }")
    body.append("    return history;")
    
    # Combine everything into the final function string
    func_body = "\n".join(body)
    function = f"{func_signature} {{\n{func_body}\n}}"
    
    return function
