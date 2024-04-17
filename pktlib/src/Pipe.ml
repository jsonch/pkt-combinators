open Syntax (* User-facing functions *)
(*
type loc = Loc.t
type locset = loc list
*)

(* a pipe is just an annotated pipe annotated with units *)
type ('i) pipe = ('i) Syntax.pipe
type arg = Syntax.arg
type args = Syntax.args

let instantiate (a : ('s, 'i) atom) (state : 's) : (unit, 'i) atom = 
  {
    name = a.name;
    arity = a.arity;
    f = (fun _ arguments map -> (a.f state arguments map));
    state_init = (fun _ -> ());
  }
(*Atom type*)
let shared_atom a = 
  let the_state_instance = a.state_init () in
  (fun _ -> instantiate a the_state_instance);;
(*** primitive combinators ***)

let atom : (string -> int -> (unit -> 's) -> ('s -> args -> 'i ArgMap.t -> 'i) -> ('s, 'i) atom) = 
  fun name arity state_init f -> 
  {
  name = name;
  arity = arity;
  f = f;
  state_init = state_creator;
  };

let atom_pipe : (('s, 'i) atom -> args -> ('i) pipe) = 
  fun a args -> Atom((), a, args)
fun name arity state_creator f args -> let a =  in Atom ((), a, args)
let let_pipe : (arg -> ('i) pipe -> ('i) pipe -> ('i) pipe) = fun arg p1 sub_pipe -> Let ((), arg, p1, sub_pipe)
let copy n : ('i) pipe = Copy((), n)
let move locs : ('i) pipe = Move((), locs)
let sequence p1 p2 : ('i) pipe = Sequence((), p1, p2)
let (>>>) = sequence
let parallel p1 p2 : ('i) pipe = Parallel((), p1, p2)
let (|||) = parallel
let shared p : ('i) pipe = Share((), p)
(*let unshared : ('x, 'y) pipe = Share((), false)*)
let locate locs pipe : ('i) pipe = Locate((), locs, pipe)
let shard = move

(*** composite / convenience combinators ***)
(*let const_move l = (* move every packet to a single location *)
  let cmove = atom "const_move" (fun x -> (x, l)) in
  sequence cmove (move [l])
;;
*)
let at l p = (locate [l] p) (* indicate where the pipeline begins *)

(*let start l = locate [l]*)

(*let rec parallel_shared : type x y. locset -> (x, y) pipe -> (x, y) pipe = 
  fun locs pipe -> 
  match locs with
  | [] -> failwith "cannot parallelize a pipeline across no cores"
  | [loc] -> shared >>> sequence (locate [loc]) pipe
  | l1::locs -> parallel (sequence (locate [l1]) pipe) (parallel_shared locs pipe)
;;

let rec parallel_unshared : type x y. locset -> (x, y) pipe -> (x, y) pipe = 
  fun locs pipe -> 
  match locs with
  | [] -> failwith "cannot parallelize a pipeline across no cores"
  | [loc] -> unshared >>> sequence (locate [loc]) pipe
  | l1::locs -> parallel (sequence (locate [l1]) pipe) (parallel_unshared locs pipe)
;;
*)
let to_string = Syntax.to_string ;;

