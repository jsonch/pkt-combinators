(* The base AST for the packet combinators library *)
type loc = Loc.t
type locset = loc list

type ('x, 'y) atom = 
  { name : string; 
    f : 'x -> 'y;
  }

type ('a, 'x, 'y) annotated_pipe = 
  | Atom        : 'a * ('x, 'y) atom -> ('a, 'x , 'y) annotated_pipe (* construct an atomic pipeline *)  
  | Copy        : 'a * int -> ('a, 'x, 'x) annotated_pipe (* make n copies of the packet *)
  | Locate      : 'a * locset -> ('a, 'x, 'x) annotated_pipe (* start a pipeline at a location *)
  | Move        : 'a * locset -> ('a, ('x * loc) , 'x) annotated_pipe (* move each packet to a location *)
  | Share       : 'a * bool -> ('a, 'x, 'x) annotated_pipe (* toggle state sharing for subsequent pipeline *)
  | Sequence    : 'a * ('a, 'x , 'i) annotated_pipe * ('a, 'i , 'y) annotated_pipe -> ('a, 'x , 'y) annotated_pipe (* run two pipelines in sequence *)
  | Parallel    : 'a * ('a, 'x , 'y) annotated_pipe * ('a, 'x , 'y) annotated_pipe -> ('a, 'x , 'y) annotated_pipe (* run two pipelines in parallel *)
;;

let rec to_string : type x y. (_, x, y) annotated_pipe -> string = function
| Atom (_, a) -> "Atom " ^ a.name
| Copy (_, i) -> "Copy " ^ (string_of_int i)
| Move (_, locs) -> "Move " ^ (Loc.to_strings locs)
| Sequence (_, p1, p2) -> "(" ^ (to_string p1) ^ " >>> " ^ (to_string p2) ^ ")"
| Parallel (_, p1, p2) -> "(" ^ (to_string p1) ^ " ||| " ^ (to_string p2) ^ ")"
| Share (_, b) -> "Share " ^ (string_of_bool b)
| Locate (_, locs) -> "Start " ^ (Loc.to_strings locs)
;;

type ('x, 'y) pipe = (unit, 'x, 'y) annotated_pipe

let to_unit_pipe p = p;;
let from_unit_pipe p = p;;
