(* a bit of cleanup. 
  Added location sets, a primitive location combinator, and parallel / shared combinators.   
  Fixed some bugs in eval and checking.
*)
(*  
  next steps: 
  1. modularize
  2. add state to atoms 
*)

(* type processor = 
  | Switch 
  | NIC
  | CPU

type location = 
  (* on a specific core of a processor *)
  | Core of processor * int
  (* somewhere on a processor *)
  | Processor of processor 
  (* anywhere *)
  | Unbound *)

type loc = 
  | Switch 
  | NIC
  | Core of int
  | Unbound

(* sets of locations *)
module Loc = struct
  type t = loc
  let compare = Stdlib.compare
end

module LocSet = Set.Make(Loc)
type locset = LocSet.t

let singleton = LocSet.singleton
let locset l = LocSet.of_list l

let locset_union locsets = List.fold_left (fun locs loc -> LocSet.union locs loc) LocSet.empty locsets;;

let string_of_loc loc = 
  match loc with 
  | Switch -> "Switch"
  | NIC -> "NIC"
  | Core i -> "Core "^(string_of_int i)
  | Unbound -> "Unbound"
;;
let string_of_locset (locs : locset) = 
  LocSet.elements locs |> List.map string_of_loc |> String.concat ", "
;;

type ('x, 'y) atom = 
{ name : string; 
  f : 'x -> 'y;
}

(* pipeline constructors are annotated with locations *)
type _ pipe =  
  | Atom : loc * ('x, 'y) atom -> ('x * 'y) pipe (* an atom is a pipeline *)
  | Compose : ('x * 'y) pipe * ('y * 'z) pipe -> ('x * 'z) pipe (* a composition of pipelines is a pipeline *)
  | Shard : loc * locset -> (('x * loc) * 'x) pipe (* a "shard" command is a pipeline that copies data 
       from a source location to a subset of destination locations, where each input is 
       annotated with its intended dest locations *)     
  | Bind : locset * ('x * 'y) pipe -> ('x * 'y) pipe  (* a pipeline bound to some locations is a pipeline *)
  | Vector : ('x * 'y) pipe list -> ('x * 'y) pipe (* a vector of parallel pipelines is a pipeline *)
;;
let rec pipe_to_string : type a b. (a * b) pipe -> string = function
  | Atom(_, {name; f}) -> Printf.sprintf "Atom %s" name
  | Compose(p1, p2) -> Printf.sprintf "%s >>> %s" (pipe_to_string p1) (pipe_to_string p2)
  | Shard(src_loc, dst_locs) -> Printf.sprintf "Shard (%s, [%s])" (string_of_loc src_loc) (string_of_locset dst_locs)
  | Bind(locs, pipe) -> Printf.sprintf "Bind ([%s], %s)" (string_of_locset locs) (pipe_to_string pipe)
  | Vector(pipes) -> Printf.sprintf "Vector [%s]" (List.map pipe_to_string pipes |> String.concat ", ")
;;

(* user-facing combinators *)
let atom name f = Atom(Unbound, {name; f})
let compose p1 p2 = Compose(p1, p2)
let (>>>) = compose
let bind locs pipe = Bind(locs, pipe)
let (@@@) p l = bind (singleton l) p
let vector pipes = Vector(pipes)
let shard locs = Shard(Unbound, locs)

let parallel (locs : locset) pipe = 
  let pipes = List.map (fun loc -> bind (singleton loc) pipe) (LocSet.elements locs) in
  vector pipes
;;
let shared = bind


(* pipeline location checking. *)
let infer_loc atom_name pipe_ends next_pipe_start = 
  let next_pipe_start' = 
    match (pipe_ends, next_pipe_start) with 
    (* case: pipeline is emitting to one location, where the atom is placed. *)
    | locs, loc when (LocSet.mem loc locs) -> loc
    (* case: pipeline emits to a single unspecified location *)
    | locs, loc when (LocSet.equal (singleton Unbound) locs) -> loc
    (* case: pipeline emits to one location, the atom is not placed *)         
    | locs, Unbound when ((LocSet.cardinal locs) = 1) -> List.hd (LocSet.elements locs)
    (* pipeline emits to multiple locations and the atom doesn't specify a single  *)
    | locs, _ -> failwith@@"a pipeline expecting multiple processors got a single atom at "^atom_name
  in
  next_pipe_start'
;;

let rec start_locs_of_pipe : type a b.
(a * b) pipe -> locset = 
fun pipe -> match pipe with
  | Atom(loc, _) -> singleton loc
  | Shard(src_loc, _) -> singleton src_loc
  | Vector(pipes) -> locset_union (List.map start_locs_of_pipe pipes)
  | Compose(p1, p2) -> start_locs_of_pipe p1
  | Bind(start_locs, pipe) -> start_locs
;;


let rec check_pipe_loc : type a b. 
    locset -> (a * b) pipe -> 
    locset * (a * b) pipe = 
  fun pipe_locs pipe -> 
    match pipe with 
  | Atom(atom_loc, a) -> 
    (* we need to figure out a specific location for this atom, and 
       check that it is consistent with the current location of the pipeline. *)
    let atom_loc' = infer_loc a.name pipe_locs atom_loc in
    (* the pipeline must continue from the atom's location *)
    (singleton atom_loc', Atom(atom_loc', a))
  | Shard(src_loc, dst_locs) -> 
    (* check / infer the start location of shard *)
    let src_loc = infer_loc "_SHARD_" pipe_locs src_loc in
    dst_locs, Shard(src_loc, dst_locs)
  | Bind(start_locs, pipe) -> 
    (* make sure that the inner pipe is unbound *)
    let inf_start_locs = start_locs_of_pipe pipe in
    if (not (LocSet.equal inf_start_locs (singleton Unbound))) then 
      failwith "location error: Bind attempts to bind a pipe that is already bound";
    (*  now, check the pipe locations of the inner pipe, which will bind them. *)    
    let inner_result = List.map (fun loc -> check_pipe_loc (singleton loc) pipe) (LocSet.elements start_locs) in 
    (* get the end locations of the bound pipe, which informs the next pipeline *)
    let end_locs = List.map fst inner_result in
    let end_locs = locset_union end_locs in
    end_locs, Bind(start_locs, pipe)
  | Vector(pipes) -> 
    (* vector must define a pipe that starts at every location where the current pipe ends *) 
    let rec check_inner pipes = 
      match pipes with 
      | [] -> LocSet.empty, LocSet.empty, []
      | pipe::pipes -> 
        let end_locs, pipe = check_pipe_loc pipe_locs pipe in
        let start_locs = start_locs_of_pipe pipe in
        let more_start_locs, more_end_locs, more_pipes = check_inner pipes in
        let start_locs = locset_union [start_locs; more_start_locs] in
        let end_locs = locset_union [end_locs; more_end_locs] in
        let pipes = pipe::more_pipes in
        start_locs, end_locs, pipes
    in
    let start_locs, end_locs, pipes = check_inner pipes in
    (* if the pipe ends at pipe_locs, then start_locs must match it. 
       After the vector, we end at the end locations of the pipes in the vector. *)
    if (LocSet.equal start_locs pipe_locs) then (end_locs, Vector(pipes))
      else failwith "location mismatch: a vector does not contain pipes that cover all the locations where the preceeding pipe ends" 
  | Compose (p1, p2) ->
    (* check p1, then check p2 given p1 *)
    let pipe_locs1, p1 = check_pipe_loc pipe_locs p1 in
    let pipe_locs2, p2 = check_pipe_loc pipe_locs1 p2 in
    (* the output location is the location _after_ p2 is done, I think *)
    pipe_locs2, Compose(p1, p2)
;;


let rec eval_pipe  : type a b. 
    loc -> (a * b) pipe -> a ->
  (loc * b) = 
  fun current_loc pipe a -> 
  match pipe with   
  | Atom(loc', {name; f}) -> current_loc, (f a)        
  | Shard(_, _) -> snd a, fst a
  | Vector(pipes) -> 
    (* find the pipe for the current location and recurse on it *)
    let rec eval_inner pipes = 
      match pipes with 
      | [] -> failwith@@"vector processing error: could not find a pipe for location: "^(string_of_loc current_loc)
      | pipe::pipes -> 
        let locs = start_locs_of_pipe pipe in
        if (LocSet.mem current_loc locs) then eval_pipe current_loc pipe a
        else eval_inner pipes
    in 
    eval_inner pipes 
  | Compose(p1, p2) -> 
    let end_p1_loc, b = eval_pipe current_loc p1 a in
    eval_pipe end_p1_loc p2 b
  | Bind(_, pipe) -> 
    (* locate just recurses -- first arg is just for loc checking *)
    eval_pipe current_loc pipe a
;;

let try_check pipe = 
  try 
    let _, pipe = check_pipe_loc LocSet.empty pipe in
    print_endline ("success: "^pipe_to_string pipe);
    pipe
  with Failure s -> print_endline ("fail: "^s); pipe
;;


(* example programs *)
type rawpkt = {buf : bytes}
type pkt = {s : int; d : int}

let parse = atom "parse" 
  (fun (r : rawpkt) -> {s=1; d=1})
;;

let schedule = atom "schedule" 
  (fun (p : pkt) -> 
    print_endline ("running scheduler");
    (p, Core(1)))    
;;

let do_work = atom "do_work" 
  (fun (p : pkt) -> 
    print_endline ("running do_work");
    if (p.s) == 0 then {p with s = 1}
    else p
  )
;;

let prog_wrong_effect = 
  parse @@@ Switch
  >>> schedule @@@ NIC
  >>> shard (locset [Core(1); Core(2)])
  >>> do_work @@@ Core(1)
;;

let prog_right_effect = 
  parse @@@ Switch
  >>> schedule @@@ NIC
  >>> shard (locset [Core(1); Core(2)]) @@@ NIC
  >>> parallel (locset [Core(1); Core(2)]) do_work
;;

print_endline ("checking effect for prog_right_effect");;
let _, prog_right_effect = check_pipe_loc LocSet.empty prog_right_effect ;;
print_endline ("done.");;

let input = {buf = Bytes.of_string "hello"};;
let output = eval_pipe NIC prog_right_effect input;;



let double_bind = 
  bind 
    (locset [Core(1); Core(2)]) 
    (bind (locset [Core(1)]) parse)
;;

print_endline ("checking effect for double_bind");;
try_check double_bind;;

let overlapping_binds = 
  (* what's this mean? running two instances on the same core? AWFUL. *)
  vector [bind (locset [Core(1)]) parse;
            bind (locset [Core(1)]) parse]
;;

print_endline ("checking effect for overlapping_binds");;
try_check overlapping_binds;;