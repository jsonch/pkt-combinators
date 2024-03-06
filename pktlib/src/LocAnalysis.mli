(* analyze pipe locations *)
open Syntax

(* a pipeline that has start and end locations attached to it *)
type ('i) located_pipe

val infer_locations : ('i) Syntax.pipe -> (locset * locset * ('i) located_pipe)

type loc_constraint
val loc_constraint_to_string : loc_constraint -> string
val loc_constraints_to_string : loc_constraint list -> string

