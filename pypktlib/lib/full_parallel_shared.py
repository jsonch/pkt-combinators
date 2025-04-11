from .backend import *
from .syntax import *

#Split this one pipe into a shard location and a bunch of parallel comput cores, all running that pipe. 
#Assumes the same state is used from below. 
def full_parallel_shared_transform(pipe : PipeBase, compute_locations : list[str], shard_location : str):
    #create the shard atom
    decision_name = "auto_shard_decision_name"
    print(generate_initialization_function("counter"))
    print(make_shard_decider(compute_locations))
    shard_decider = do(atom("void*", "initialize_counter", "void*", "rr_shard", []), [])
    cases = {}
    for s in compute_locations:
        cases[s] = at(s, pipe)
    final_pipe = let(decision_name, shard_decider, switch(decision_name, cases))
    print(final_pipe)
    return final_pipe

def make_shard_decider(compute_locations: list[str]):
    # Calculate the modulus value, which is the length of the list
    mod_value = len(compute_locations)
    
    # Define the function signature
    func_signature = "int rr_shard(int *value)"
    
    # Generate the body of the function
    body = []
    body.append("    // Store the old value to return later")
    body.append("    int old_value = *value;")
    
    # Increment the int value modulo the length of the list
    body.append(f"    // Increment the value modulo {mod_value}")
    body.append(f"    *value = (*value + 1) % {mod_value};")
    
    # Return the old value
    body.append("    return old_value;")
    
    # Combine everything into the final function string
    func_body = "\n".join(body)
    function = f"{func_signature} {{\n{func_body}\n}}"
    
    return function

def generate_initialization_function(var_name: str) -> str:
    # Define the function signature
    func_signature = f"int* initialize_{var_name}()"
    
    # Generate the body of the function
    body = []
    body.append(f"    // Allocate memory for an integer")
    body.append(f"    int *{var_name} = (int *)malloc(sizeof(int));")
    body.append("    if ({var_name} == NULL) {")
    body.append("        // Handle malloc failure")
    body.append("        return NULL;")
    body.append("    }")
    
    body.append(f"    // Initialize the integer variable {var_name} to 0")
    body.append(f"    *{var_name} = 0;")
    
    body.append(f"    // Return the pointer to the allocated integer")
    body.append(f"    return {var_name};")
    
    # Combine everything into the final function string
    func_body = "\n".join(body)
    function = f"{func_signature} {{\n{func_body}\n}}"
    
    return function

