(* idea: 
  Annotate each pipeline constructor with a location where it runs. 
  Let ocaml type check the pipeline in / out types, 
  and write our own checker function to make sure the locations are consistent.
*)

(*  
next steps: 

0. clean things up (modules, etc.)
1. represent atoms as objects with state. 

2. to represent parallel and shared state, we can add a "location" combinator, 
   (currently commented out)
    then write combinators for parallel and shared constructors. 
  parallel state will look like:     
    Vector [
      Locate [Core 1] (Atom f1);
      Locate [Core 2] (Atom f1);
    ]

  shard state will look like: 
    Vector Locate [Core 1; Core 2] (Atom f1);
    -- note, there's only 
*)

type loc = 
  | Switch 
  | NIC
  | Core of int
  | Unspecified

let string_of_loc loc = 
  match loc with 
  | Switch -> "Switch"
  | NIC -> "NIC"
  | Core i -> "Core "^(string_of_int i)
  | Unspecified -> "Unspecified"
;;
let string_of_locs locs = 
  List.map string_of_loc locs |> String.concat ", "

(* atoms are just functions with names (disregard state for now) *)
type ('x, 'y) atom = (string * ('x -> 'y))

(* pipeline constructors are all annotated with locations *)
type _ pipe =  
  | Atom : loc * ('x, 'y) atom -> ('x * 'y) pipe (* a function to execute in the pipeline *)
  | Compose : ('x * 'y) pipe * ('y * 'z) pipe -> ('x * 'z) pipe (* compose calls the second pipe with the output from the first. *)
  (* | Locate : loc * ('x * 'y) pipe -> ('x * 'y) pipe  *)
  (* shard's location annotation represents all the possible locations that it could output to *)
  | Shard : loc * loc list -> (('x * loc) * 'x) pipe (* shard a pipeline into multiple pipelines, each with a different location *)  
  (* vector's location represents all the possible locations where the inner pipes could be placed *)
  | Vector : ('x * 'y) pipe list -> ('x * 'y) pipe (* a vector of parallel pipes that do the same transformation *)
;;

let compose p1 p2 = Compose(p1, p2)
let (>>>) = compose

let atom a = Atom(Unspecified, a)
let located_atom loc a = Atom(loc, a)
let shard loc locs = Shard(loc, locs)
let vector pipes = Vector(pipes)


(* PROGRAM *)
type rawpkt = {buf : bytes}
type pkt = {s : int; d : int}

let parse (r : rawpkt) : pkt = let _ = r in 
  print_endline ("running parser");
  {s=1; d=1};;

let schedule (p : pkt) : (pkt * loc) = 
  print_endline ("running scheduler");
  (p, Core(1))
;;
let do_work (pkt : pkt) : pkt = 
  print_endline ("running do_work");
  if (pkt.s) == 0 then {pkt with s = 1}
  else pkt



let is_specific_loc loc = 
  match loc with 
  | Switch 
  | NIC 
  | Core _ -> true
  | _  -> false
;;

let infer_atom_loc atom_name pipe_locs atom_loc = 
  let atom_loc' = 
    match (pipe_locs, atom_loc) with 
    (* case: pipeline is emitting to one location, where the atom is placed. *)
    | [loc], loc' when (loc = loc') -> loc
    (* case: pipeline emits to a single unspecified location *)
    | [Unspecified], loc -> loc
    (* case: pipeline emits to one location, the atom is not placed *)         
    | [loc], Unspecified -> loc
    (* pipeline emits to multiple locations. A single atom can never handle this, it is on 1 location. *)
    | locs, _ -> failwith@@"a pipeline expecting multiple processors got a single atom at "^atom_name
  in
  atom_loc'
;;

let rec check_pipe_loc : type a b. 
    loc list -> (a * b) pipe -> 
    loc list * (a * b) pipe = 
  fun pipe_locs pipe -> match pipe with 
  | Atom(atom_loc, a) -> 
    (* we need to figure out a specific location for this atom, and 
       check that it is consistent with the current location of the pipeline. *)
    let atom_loc' = infer_atom_loc (fst a) pipe_locs atom_loc in
    (* the rest of the pipeline must continue from the atom's location *)
    ([atom_loc'], Atom(atom_loc', a))
  | Shard(src_loc, dst_locs) -> 
    (* Shard "moves" data from a src_loc to something in dst_locs *)
    (* first, we have to see if we can figure out where the shard op actually runs. *)
    let src_loc = infer_atom_loc "_SHARD_" pipe_locs src_loc in
    (* now, the pipeline continues at the dst_loc, not the src loc *)
    dst_locs, Shard(src_loc, dst_locs)
  | Vector(pipes) -> 
    (* vector must define a pipe for every location where the current pipe might run *)
    let locs_and_pipes = List.map2 (check_pipe_loc) (List.map (fun p -> [p]) pipe_locs) pipes in
    (* this should be an intersection of sets, not flatten *)
    let locs = List.map fst locs_and_pipes |> List.flatten in
    let pipes = List.map snd locs_and_pipes in
    if (List.equal (=) locs pipe_locs) then (locs, Vector(pipes))
      else failwith "location mismatch: vector defines pipes on a different set of cores than it was declared over"
  | Compose (p1, p2) ->
    (* check p1, then check p2 given p1 *)
    let pipe_locs1, p1 = check_pipe_loc pipe_locs p1 in
    let pipe_locs2, p2 = check_pipe_loc pipe_locs1 p2 in
    (* the output location is the location _after_ p2 is done, I think *)
    pipe_locs2, Compose(p1, p2)
;;


let parse = ("parse", parse);;
let schedule = ("schedule", schedule);;
let do_work = ("do_work", do_work);;


let do_work_wrong (pkt : rawpkt) : rawpkt = 
  print_endline ("running do_work");
  pkt
;;

(* 
  uncomment this 
let prog_wrong_types = 
  (located_atom NIC parse)
  >>>
  (located_atom NIC schedule)
  >>> 
  (shard NIC [Core(1); Core(2)])
  >>>
  (located_atom (Core(1)) do_work_wrong)
;; *)


let prog_wrong_effect = 
  (located_atom NIC parse)
  >>> 
  (located_atom NIC schedule)
  >>> 
  (shard NIC [Core(1); Core(2)])
  >>>
  (located_atom (Core(1)) do_work)
;;

let prog_right_effect = 
  located_atom NIC parse
  >>> 
  located_atom NIC schedule
  >>> 
  shard NIC [Core(1); Core(2)]
  >>>
  vector
    [located_atom (Core(1)) do_work; located_atom (Core(2)) do_work]
;;

(* this passes the effect checking, because all the output locations 
     of shard have pipelines defined on them *)
;;

(*  
   
located_atom Core(1) schedule 
>>> 

*)

let prog_preprocess = 
  located_atom NIC parse
  >>> 
  located_atom NIC schedule
  >>> 
  shard NIC [Core(1); Core(2)]
;;

let prog_main =   vector 
[located_atom (Core(1)) do_work; located_atom (Core(2)) do_work]
;;

let prog_right_effect2 = 
  prog_preprocess >>> prog_main
;;


print_endline ("checking effect for prog_right_effect");;
let _, prog_right_effect = check_pipe_loc [NIC] prog_right_effect ;;
print_endline ("done.");;


(* print_endline ("checking effect for prog_wrong_effect");;
let _ = check_pipe_loc [NIC] prog_wrong_effect ;;

print_endline ("done.");; *)


let rec loc_of_pipe : type a b. 
  (a * b) pipe -> loc = 
  fun pipe -> match pipe with
    | Atom(loc, _) -> loc
    | Shard(src_loc, _) -> src_loc
    | Vector(pipes) -> failwith "[loc_of_pipe] a vector does not have 1 location"
    | Compose(p1, p2) -> loc_of_pipe p2
;;

let rec eval_pipe  : type a b. 
    loc -> (a * b) pipe -> a ->
  (loc * b) = 
  fun loc pipe a -> match pipe with 
  (* eval atom, update loc (even though update loc isn't necessary bc its an atom) *)
  | Atom(loc', (name, f)) -> loc', (f a)      
  | Shard(src_loc, dst_loc) -> 
    (* shard just changes the location based on a! *)
    let a, loc' = fst a, snd a in
    (loc', a)
  | Vector(pipes) -> 
    (* for a vector, we just find the current location and run the pipe *)
    let pipe = List.find (fun p -> loc_of_pipe p = loc) pipes in
    eval_pipe loc pipe a
  | Compose(p1, p2) -> 
    let loc, b = eval_pipe loc p1 a in
    eval_pipe loc p2 b
;;


let input = {buf = Bytes.of_string "hello"};;
let output = eval_pipe NIC prog_right_effect input;;


print_endline ("checking effect for prog_right_effect2");;
let _, prog_right_effect2 = check_pipe_loc [NIC] prog_right_effect2 ;;
print_endline ("done.");;

let output2 = eval_pipe NIC prog_right_effect2 input;;
