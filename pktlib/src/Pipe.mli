open Syntax(* Pipeline combinators *)

(** [('x, 'y) pipe] is a pipeline from x's to y's *)
type ('i) pipe = ('i) Syntax.pipe
type arg = Syntax.arg
type args = Syntax.args

(** [id] creates an identity pipe. *)

(** [atom name f] creates an atomic pipe with the given name and function. *)
val atom : string -> int -> (unit -> 's) -> ('s -> args -> 'i ArgMap.t -> 'i) -> args -> ('i) pipe
val let_atom : string -> int -> (unit -> 's) -> ('s -> args -> 'i ArgMap.t -> 'i) -> args -> arg -> ('i) pipe -> ('i) pipe
val instantiate : ('s, 'i) atom -> 's -> ('i instantiated_atom)
val shared_atom : ('s, 'i) atom -> (unit -> ('i instantiated_atom))

(* [copy n] creates a pipe that copies its input [n] times. *)
val copy : int -> ('i) pipe

(** [move locs] creates a pipe that moves its input to the given locations. 
val move : Loc.t list -> ('i) pipe*)

(** [sequence p1 p2] creates a pipe that applies [p1] to its input, then applies [p2] to the output of [p1]. *)
val sequence : ('i) pipe -> ('i) pipe -> ('i pipe)

(** [>>>] sequence operator **)
val (>>>) : ('i) pipe -> ('i) pipe -> ('i pipe)

(** [parallel p1 p2] creates two task-parallel pipes  **)
val parallel : ('i) pipe -> ('i) pipe -> ('i pipe)

(** [|||] parallel operator  **)
val (|||) : ('i) pipe -> ('i) pipe -> ('i pipe)

(** [shared] creates a pipe that shares its input across all available cores. *)
val shared : ('i) pipe -> ('i) pipe

(* [unshared] creates a pipe that does not share its input across cores. 
val unshared : ('x, 'x) pipe
*)
(** [locate loc] creates a pipe that starts at the given locations. *)
val locate : Loc.t list -> ('i pipe) -> ('i) pipe

(** [at loc p] create a pipe p that starts at loc *)
val at : Loc.t -> ('i) pipe -> ('i) pipe

(*val start : Loc.t -> ('x, 'x) pipe*)


(** [shard locs] alias for move *)
val shard : Loc.t list -> ('i) pipe

(* [const_move] move to a fixed location 
val const_move : Loc.t -> ('i) pipe*)
(*Commenting out some things I don't know how to do yet TODO
(* [parallel_shared locs pipe] creates a pipeline that applies [pipe] to its input in parallel across the given locations, sharing the input across all cores. *)
val parallel_shared : Loc.t list -> ('i) pipe -> ('i) pipe

(* [parallel_unshared locs pipe] creates a pipeline that applies [pipe] to its input in parallel across the given locations, without sharing the input across cores. *)
val parallel_unshared : Loc.t list -> ('i) pipe -> ('i) pipe
*)
(** [to_string pipe] returns a string representation of the given pipeline. *)
val to_string : ('i) pipe -> string

