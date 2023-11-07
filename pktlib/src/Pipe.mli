(* Pipeline combinators *)

(** [('x, 'y) pipe] is a pipeline from x's to y's *)
type ('x, 'y) pipe = ('x, 'y) Syntax.pipe

(** [id] creates an identity pipe. *)

(** [atom name f] creates an atomic pipe with the given name and function. *)
val atom : string -> ('x -> 'y) -> ('x, 'y) pipe

(** [copy n] creates a pipe that copies its input [n] times. *)
val copy : int -> ('x, 'x) pipe

(** [move locs] creates a pipe that moves its input to the given locations. *)
val move : Loc.t list -> ('x * Loc.t, 'x) pipe

(** [sequence p1 p2] creates a pipe that applies [p1] to its input, then applies [p2] to the output of [p1]. *)
val sequence : ('x, 'i) pipe -> ('i, 'y) pipe -> ('x, 'y) pipe

(** [>>>] sequence operator **)
val (>>>) : ('x, 'i) pipe -> ('i, 'y) pipe -> ('x, 'y) pipe

(** [parallel p1 p2] creates two task-parallel pipes  **)
val parallel : ('x, 'y) pipe -> ('x, 'y) pipe -> ('x, 'y) pipe

(** [|||] parallel operator  **)
val (|||) : ('x, 'y) pipe -> ('x, 'y) pipe -> ('x, 'y) pipe

(** [shared] creates a pipe that shares its input across all available cores. *)
val shared : ('x, 'x) pipe

(** [unshared] creates a pipe that does not share its input across cores. *)
val unshared : ('x, 'x) pipe

(** [locate loc] creates a pipe that starts at the given locations. *)
val locate : Loc.t list -> ('x, 'x) pipe

(** [at loc p] create a pipe p that starts at loc *)
val at : Loc.t -> ('x, 'y) pipe -> ('x, 'y) pipe

val start : Loc.t -> ('x, 'x) pipe


(** [shard locs] alias for move *)
val shard : Loc.t list -> ('x * Loc.t, 'x) pipe

(** [const_move] move to a fixed location *)
val const_move : Loc.t -> ('x, 'x) pipe

(** [parallel_shared locs pipe] creates a pipeline that applies [pipe] to its input in parallel across the given locations, sharing the input across all cores. *)
val parallel_shared : Loc.t list -> ('x, 'y) pipe -> ('x, 'y) pipe

(** [parallel_unshared locs pipe] creates a pipeline that applies [pipe] to its input in parallel across the given locations, without sharing the input across cores. *)
val parallel_unshared : Loc.t list -> ('x, 'y) pipe -> ('x, 'y) pipe

(** [to_string pipe] returns a string representation of the given pipeline. *)
val to_string : ('x, 'y) pipe -> string

