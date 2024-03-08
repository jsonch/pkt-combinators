(* The base AST for the packet combinators library *)
type loc = Loc.t
type locset = loc list


type arg = String.t (*The name of the args to call with.*)
type args = arg list

module ArgMap = Map.Make(String);;
(*The type of state and the type of input *)
type ('s, 'i) atom = 
  { name : string; 
    arity : int;
    f : 's -> args -> 'i ArgMap.t -> 'i;
    state_init : unit -> 's
  }
(*Just an atom that has had its state created*)
type ('i) instantiated_atom = (unit, 'i) atom

  
(*Atoms return values. Pipe's don't have to?*)
type ('a, 'i) annotated_pipe = 
  | Atom        : 'a * (_, 'i) atom * arg list -> ('a, 'i) annotated_pipe (* construct an atomic pipeline *)
  | Let         : 'a * arg * (('a, 'i) annotated_pipe) * (('a, 'i) annotated_pipe) -> ('a, 'i) annotated_pipe (*let x = p1 in p2*)
  | Copy        : 'a * int -> ('a, 'i) annotated_pipe (* make n copies of the packet *)
  | Locate      : 'a * locset  * ('a, 'i) annotated_pipe -> ('a, 'i) annotated_pipe (* start a pipeline at a location *)
  | Move        : 'a * locset -> ('a, 'i) annotated_pipe (* move each packet to a location *)
  | Share       : 'a * ('a, 'i) annotated_pipe -> ('a, 'i) annotated_pipe (* toggle state sharing for subsequent pipeline *)
  | Sequence    : 'a * ('a, 'i) annotated_pipe * ('a, 'i) annotated_pipe -> ('a, 'i) annotated_pipe (* run two pipelines in sequence *)
  | Parallel    : 'a * ('a, 'i) annotated_pipe * ('a, 'i) annotated_pipe -> ('a, 'i) annotated_pipe (* run two pipelines in parallel *)
;;

type ('a, 'i) instantiated_pipe = 
| Atom        : 'a * ('i) instantiated_atom * arg list -> ('a, 'i) instantiated_pipe (* construct an atomic pipeline *)
| Let         : 'a * ('i) instantiated_atom * arg list * arg * (('a, 'i) instantiated_pipe) -> ('a, 'i) instantiated_pipe (*let x = atom ( args) in pipe*)
| Copy        : 'a * int -> ('a, 'i) instantiated_pipe (* make n copies of the packet *)
| Locate      : 'a * locset  * ('a, 'i) instantiated_pipe -> ('a, 'i) instantiated_pipe (* start a pipeline at a location *)
| Move        : 'a * locset -> ('a, 'i) instantiated_pipe (* move each packet to a location *)
| Share       : 'a * ('a, 'i) instantiated_pipe -> ('a, 'i) instantiated_pipe (* toggle state sharing for subsequent pipeline *)
| Sequence    : 'a * ('a, 'i) instantiated_pipe * ('a, 'i) instantiated_pipe -> ('a, 'i) instantiated_pipe (* run two pipelines in sequence *)
| Parallel    : 'a * ('a, 'i) instantiated_pipe * ('a, 'i) instantiated_pipe -> ('a, 'i) instantiated_pipe (* run two pipelines in parallel *)
;;

let print_list l = (List.fold_left (fun s acc -> acc ^ ","^s) "(" l) ^")"

let rec to_string : ('a, 'i) annotated_pipe -> string = function
| Atom (_, a, args) -> Printf.sprintf "Atom %s %s" a.name (print_list args)
| Let (_, a, args, var, p) -> Printf.sprintf "let %s = %s%s in %s" var a.name (print_list args) (to_string p) 
| Copy (_, i) -> "Copy " ^ (string_of_int i)
| Move (_, locs) -> "Move " ^ (Loc.to_strings locs)
| Sequence (_, p1, p2) -> "(" ^ (to_string p1) ^ " >>> " ^ (to_string p2) ^ ")"
| Parallel (_, p1, p2) -> "(" ^ (to_string p1) ^ " ||| " ^ (to_string p2) ^ ")"
| Share (_, p) -> "Share " ^ (to_string p)
| Locate (_, locs, p) -> "Locate " ^ (Loc.to_strings locs) ^ (to_string p)
;;

type ('i) pipe = (unit, 'i) annotated_pipe

(* This should work but I can't figure out why it doesn't type check
let func_wrapper (arity:int) f = 
  let rec wrapped_acc (arguments : args) (map : 'i ArgMap.t) f_acc =
    match arguments with 
    | [] -> f_acc
    | h :: t -> (wrapped_acc t map (f_acc (ArgMap.get h map))) in
  fun (state:'s) (arguments:args) (map : 'i ArgMap.t) ->
    wrapped_acc arguments map (f state)
*)


let to_unit_pipe p = p;;
let from_unit_pipe p = p;;
