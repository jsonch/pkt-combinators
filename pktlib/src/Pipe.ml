(* User-facing functions *)

type loc = Loc.t
type locset = loc list


(* a pipe is just an annotated pipe annotated with units *)
type ('x, 'y) pipe = ('x, 'y) Syntax.pipe

type ('x, 'y) atom = ('x, 'y) Syntax.atom

(*** primitive combinators ***)

let atom obj : ('x, 'y) pipe = Atom((), obj)
let copy n : ('x, 'x) pipe = Copy((), n)
let move locs : ('x * loc, 'x) pipe = Move((), locs)
let sequence p1 p2 : ('x, 'y) pipe = Sequence((), p1, p2)
let (>>>) = sequence
let parallel p1 p2 : ('x, 'y) pipe = Parallel((), p1, p2)
let (|||) = parallel
let shared : ('x, 'y) pipe = Share((), true)
let unshared : ('x, 'y) pipe = Share((), false)
let locate locs : ('x, 'x) pipe = Locate((), locs)
let shard = move

let stateless_atom name f = atom
  object (_)
    method get_name = name
    method f = f
  end

(*** composite / convenience combinators ***)
let const_move l = (* move every packet to a single location *)
  let cmove = stateless_atom "const_move" (fun x -> (x, l))
  in
  sequence cmove (move [l])
;;



let at l p = (locate [l] >>> p) (* indicate where the pipeline begins *)

let start l = locate [l]

let rec parallel_shared : type x y. locset -> (x, y) pipe -> (x, y) pipe = 
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

let to_string = Syntax.to_string ;;

