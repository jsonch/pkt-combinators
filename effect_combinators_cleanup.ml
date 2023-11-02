(* 
  clean up and simplify effect_combinators_cleanup

*)


module type PipeSig = sig
  type loc
  type ('x, 'y) atom
  type _ pipe
  (* pipeline combinators *)

  (* construct an atomic pipe *)
  val atom : string -> ('x -> 'y) -> ('x * 'y) pipe (* name * atom -> "atomic" pipeline *)
  (* pin a pipeline to a location. e.g., "locate Switch parse" pins an instance of "parse" to Switch *)
  val locate : loc -> ('x * 'y) pipe -> ('x * 'y) pipe 
  (* compose two pipelines together *)
  val compose : ('x * 'y) pipe -> ('y * 'z) pipe -> ('x * 'z) pipe
  (* construct input-parallel pipelines that runs concurrently at multiple 
     locations, with each pipeline using either local memory (parallel) 
     or shared memory (shared). *)
  val parallel : loc -> ('x * 'y) pipe -> ('x * 'y) pipe
  val shared : loc -> ('x *'y) pipe -> ('x * 'y) pipe

  (* Builtin pipelines *)
  (* given a target location, construct a pipeline that moves 
     each packet from the current location to the target location  *)
  val move_to : loc -> ('x * 'x) pipe

  (* given a list of possible target locations, construct a pipeline 
     that moves each packet annotated with its target locations 
     to its target locations. *)
  val shard_to : loc -> (('x * loc) * 'x) pipe

  (* operators *)
  (* ">>>" is compose *)
  val (>>>) : ('x * 'y) pipe -> ('y * 'z) pipe -> ('x * 'z) pipe
  (* "@@@" is locate *)
  val (@@@) : ('x * 'y) pipe -> loc -> ('x * 'y) pipe

  (* pipe analysis and execution *)
  val typecheck : ('x * 'y) pipe -> ('x * 'y) pipe
  val exec : ('x * 'y) pipe -> 'x -> (loc * 'y) list

  (* misc helpers *)
  val cores : int list -> loc (* take a list of int core ids and turn it into a set of "core" locations *)
  val nic : loc
  val switch : loc

  val loc_to_string : loc -> string
end


let dprint_endline = print_endline
let dprint_endline _ = ()

(* locations *)

module Loc = struct

  type t = 
    | Switch 
    | NIC
    | Core of int
    | Unbound of int
    | Locs of t list

  
  let compare loc1 loc2 = 
    match (loc1, loc2) with
    | (Switch, Switch) -> 0
    | (Switch, _) -> -1
    | (_, Switch) -> 1
    | (NIC, NIC) -> 0
    | (NIC, _) -> -1
    | (_, NIC) -> 1
    | (Core i, Core j) -> Int.compare i j
    | (Core _, _) -> -1
    | (_, Core _) -> 1
    | (Unbound i, Unbound j) -> Int.compare i j
    | (Unbound _, _) -> -1
    | (_, Unbound _) -> 1
    | (Locs l1, Locs l2) -> List.compare compare l1 l2
  ;;

  let equal loc1 loc2 = 
    compare loc1 loc2 = 0
  ;;

  (* unique and ordered list of locations *)
  let set locs = 
    List.sort_uniq compare locs
  ;;

  (* constructors *)
  let vec locs = 
    Locs (set locs)
  ;;
  let cores ints = 
    vec (List.map (fun i -> Core i) ints)

  let rec flatten locs = 
    match locs with
    | [] -> []
    | (Locs locs') :: locs -> (flatten locs') @ (flatten locs)
    | loc :: locs -> loc :: (flatten locs)
  ;;
  let has_dup = function
    | [] -> false
    | x :: xs -> List.mem x xs
  ;;

  (* get the intersection of two loc lists, 
     i.e., the locs that are present in both *)
  let intersection locs1 locs2 = 
    List.filter (fun loc -> List.mem loc locs2) (set locs1)
  ;;

  let union locs1 locs2 = 
    set (locs1 @ locs2)
  ;;
  (* a fresh unbound location *)
  let ct = ref 0
  let fresh () = 
    ct := !ct + 1;
    Unbound !ct
  ;;

  (* refresh a location if it is unbound *)
  let refresh loc = match loc with
    | Unbound _ -> fresh ()
    | _ -> loc
  ;;

  let rec loc_to_string loc = 
    match loc with
    | Switch -> "Switch"
    | NIC -> "NIC"
    | Core i -> "Core " ^ string_of_int i
    | Unbound i -> "Unbound " ^ string_of_int i
    | Locs locs -> "Locs [" ^ (String.concat ", " (List.map loc_to_string locs)) ^ "]"
  ;;
  
  let to_list loc = 
    match loc with
    | Locs locs -> locs
    | _ -> [loc]
  ;;

  let rec length loc = 
    match loc with
    | Switch -> 1
    | NIC -> 1
    | Core _ -> 1
    | Unbound _ -> 1
    | Locs locs -> List.fold_left (fun acc loc -> acc + (length loc)) 0 locs
  ;;

  (* is a location a subset of another location? *)
  let is_subset loc locs = 
    let flat_locs = flatten locs in
    let flat_loc = flatten [loc] in
    List.for_all (fun loc -> List.mem loc flat_locs) flat_loc
  ;;


end




module Pipe : PipeSig = struct 
  type loc = Loc.t
  let loc_to_string = Loc.loc_to_string
  let cores = Loc.cores
  let nic = Loc.NIC
  let switch = Loc.Switch
  type ('x, 'y) atom = 
  { name : string; 
    f : 'x -> 'y;
  }
  
  type _ pipe =  
    | Atom : loc * ('x, 'y) atom -> ('x * 'y) pipe
    | Locate : loc * ('x * 'y) pipe list -> ('x * 'y) pipe (* bind a pipe to a location *)
    | Move   : loc * loc -> ('x * 'x) pipe (* move from one location to another *)
    | Shard  : loc * loc -> (('x * loc) * 'x) pipe 
    | Vector : ('x * 'y) pipe list -> ('x * 'y) pipe
    | Compose : ('x * 'y) pipe * ('y * 'z) pipe -> ('x * 'z) pipe
  ;;

  let rec pipe_to_string : type a b. (a * b) pipe -> string = function
    | Atom(l, a) -> "Atom(" ^ (Loc.loc_to_string l) ^ ", " ^ a.name ^ ")"
    | Locate(l, p) -> "Locate(" ^ (Loc.loc_to_string l) ^ ", " ^ (pipes_to_string p) ^ ")"
    | Move(l1, l2) -> "Move(" ^ (Loc.loc_to_string l1) ^ ", " ^ (Loc.loc_to_string l2) ^ ")"
    | Shard(l1, l2) -> "Shard(" ^ (Loc.loc_to_string l1) ^ ", " ^ (Loc.loc_to_string l2) ^ ")"
    | Vector(ps) -> "Vector([" ^ (String.concat ", " (List.map pipe_to_string ps)) ^ "])"
    | Compose(p1, p2) -> "" ^ (pipe_to_string p1) ^ " >>> " ^ (pipe_to_string p2) ^ ""
  and pipes_to_string : type a b. (a * b) pipe list -> string = function
    | [] -> ""
    | hd::[] -> pipe_to_string hd
    | hd::tl -> (pipe_to_string hd)^", "^(pipes_to_string tl)
  ;;    


  (* refresh every unbound loc in a pipeline *)
  let refresh_locs pipe = 
    let rec refresh_locs : type a b. (a * b) pipe -> (a * b) pipe = function
      | Atom(l, a) -> Atom(Loc.refresh l, a)
      | Locate(l, ps) -> Locate(Loc.refresh l, List.map refresh_locs ps)
      | Move(l1, l2) -> Move(Loc.refresh l1, Loc.refresh l2)
      | Shard(l1, l2) -> Shard(Loc.refresh l1, Loc.refresh l2)
      | Vector(ps) -> Vector(List.map refresh_locs ps)
      | Compose(p1, p2) -> Compose(refresh_locs p1, refresh_locs p2)
    in
    refresh_locs pipe
  ;;  

  (* public combinator wrappers *)
  let atom name f = Atom (Loc.fresh (), {name = name; f = f})
  let locate loc p = Locate (loc, List.map (fun _ -> refresh_locs (p)) (Loc.flatten [loc]))
  let (@@@) p l = locate l p

  let shard src dsts = Shard (src, dsts)
  let shard_to dsts = Shard (Loc.fresh (), dsts)
  let vector ps = Vector ps
  let compose p1 p2 = Compose (p1, p2)
  let (>>>) = compose


  let parallel loc pipe = 
    let locs = Loc.to_list loc in
    let pipes = List.map (fun loc -> locate loc (refresh_locs pipe)) locs in
    let res = Vector pipes in
    res
  ;;
  (* shared memory pipelines *)
  let shared loc pipe = 
    let locs = Loc.to_list loc in
    let res = locate (Loc.vec locs) pipe in
    res
  ;;
  let move src dst = Move (src, dst)
  let move_to dst = move (Loc.fresh ()) dst

  let shared loc pipe = 
    let locs = Loc.to_list loc in
    let pipes = List.map (fun loc -> locate loc (refresh_locs pipe)) locs in
    let res = Vector pipes in
    res
  ;;

  type loc_equiv = (loc * loc)

  (*** location inference ***)

  let rec unify_locs l1 l2 = 
    match (l1, l2) with
    | (Loc.Unbound i, Loc.Unbound j) -> Loc.Unbound (min i j)
    | (Loc.Unbound i, _) -> l2
    | (_, Loc.Unbound i) -> l1
    (* singleton loc sets unify with singleton locs *)
    | (Loc.Locs [l1], l2)
    | (l1, Loc.Locs [l2]) -> unify_locs l1 l2
    | (Loc.Locs l1, Loc.Locs l2) -> 
      if (List.length l1) <> (List.length l2) then failwith "unification failed"
      else Loc.Locs (List.map2 unify_locs l1 l2)
    | _, _ -> if Loc.equal l1 l2 then l1 else failwith "unification failed"
  ;;

  (* replace every instance of location oldloc 
     with location newloc, for use after unification *)
  let rec replace_loc oldloc newloc = 
    let replace_loc' l = 
      if Loc.equal l oldloc then newloc else l
    in
    let rec replace_loc : type a b. (a * b) pipe -> (a * b) pipe = function
      | Atom(l, a) -> Atom(replace_loc' l, a)
      | Locate(l, ps) -> Locate(replace_loc' l, List.map replace_loc ps)
      | Move(l1, l2) -> Move(replace_loc' l1, replace_loc' l2)
      | Shard(l1, l2) -> Shard(replace_loc' l1, replace_loc' l2)
      | Vector(ps) -> Vector(List.map replace_loc ps)
      | Compose(p1, p2) -> Compose(replace_loc p1, replace_loc p2)
    in
    replace_loc
  ;;


  (* location typing rules: 

    pipe = Atom(l, a) => 
        pipe : (l, l)

    pipe = Locate(l, ps) with ps = (pi : (src_i, dst_i) for i = 1 to |l|) =>
      l == union src_1 src_2 ... src_|l|
      and pipe : (l, union dst_1 dst_2 ... dst_|l|)
    
    pipe = Shard(src, dst) =>
      pipe : (src, dst)
    
    pipe = Move(src, dst) =>
      pipe : (src, dst)

    pipe = Vector(ps) with ps : (ps1, pe1) ... (psn, pen) =>
      for all i, j (i != j), (intersect ps1 psi) == [] 
      and pipe : (union ps1 ps2 ... psn, union pe1 pe2 ... pen)

    pipe = Compose(p1, p2) with p1 : (ps1, pe1) and p2 : (ps2, pe2) =>
      pipe : (ps1, pe2)
  *)
  let n_tabs = ref 0

  (* returns start location, end location, and pipeline with unified locations *)
  let rec inf_loc : type a b. (a * b) pipe -> (loc * loc * (a * b) pipe)
    = fun pipe -> 
    (* use n_tabs to add spaces to debug string *)
    let tab () = String.make (!n_tabs * 2) ' ' in
    n_tabs := !n_tabs + 1;
    dprint_endline (tab () ^ "[inf start] "^(pipe_to_string pipe));
    let res = match pipe with 
    | Atom (l, a) -> (l, l, pipe)
    (* pipe = Locate(l, p) with p : (ps, pe) => 
        l == ps, pipe : (ps, pe) *)
    | Locate(l, ps) -> 
      (* turn into a flat list of locations *)
      let ls = Loc.flatten [l] in
      if (List.length ls) <> (List.length ps) then failwith "location inference failed in a locate -- mismatch between locations and pipes";
      (* infer the locations of the inner pipes *)      
      let located_ps = List.map2 
        (fun l p -> 
          let (p_start, p_end, p) = inf_loc p in
          let (p_start, p_end, p) = unify_and_recheck l p_start p in
          (p_start, p_end, p))
        ls ps 
      in
      let starts, ends, ps = List.fold_left (fun (starts, ends, ps) (s, e, p) -> 
        (starts@[s], ends@[e], ps@[p])) ([], [], []) located_ps
      in
      (* the located pipe goes from the union of the starts of the inner pipes to the union of the ends of the inner pipes *)
      Loc.Locs starts, Loc.Locs ends, Locate(Loc.Locs starts, ps)
    | Shard(src, dst) -> (src, dst, pipe)
    | Move(src, dst) -> (src, dst, pipe)
    | Vector (ps) -> 
      (* infer start and end locations from inner pipes *)
      let starts, ends, ps = inf_locs ps in 
      (* no unification, so no recheck needed *)
      (Locs starts, Locs ends, Vector ps)
    | Compose(p1, p2) -> 
      (* unify end of p1 with start of p2 *)
      let (ps1, pe1, p1) = inf_loc p1 in
      let (ps2, pe2, p2) = inf_loc p2 in
      let (ps1, pe1, p1) = unify_and_recheck pe1 ps2 p1 in
      let (ps2, pe2, p2) = unify_and_recheck pe1 ps2 p2 in
      (ps1, pe2, Compose(p1, p2))
    in 
    let start, fin, pipe' = res in
    dprint_endline (tab () ^ "[inf end] "^(pipe_to_string pipe') ^ " : ("^(Loc.loc_to_string start)^", "^(Loc.loc_to_string fin)^")");
    n_tabs := !n_tabs - 1;
    res

  and inf_locs : type a b. (a * b) pipe list -> (loc list * loc list * (a * b) pipe list)
    = fun pipes -> 
    let pipes_infs = List.map inf_loc pipes in
    let starts, ends, pipes = List.fold_left (fun (starts, ends, pipes) (s, e, p) -> 
      (s :: starts, e :: ends, p :: pipes)) ([], [], []) pipes_infs 
    in
    let starts, ends, pipes = List.rev starts, List.rev ends, List.rev pipes in
    let starts = Loc.flatten starts in
    let ends = Loc.flatten ends in
    if (Loc.has_dup starts) then 
      failwith "there are overlapping start locations in a vector pipeline";
    (starts, ends, pipes)
  and unify_and_recheck 
  : type a b. loc -> loc -> (a * b) pipe -> (loc * loc * (a * b) pipe)
    = fun loc1 loc2 pipe ->   
    let loc = unify_locs loc1 loc2 in
    let pipe = replace_loc loc1 loc pipe in
    let pipe = replace_loc loc2 loc pipe in
    inf_loc pipe 
  ;;
    

  (* evaluate a pipeline on a value a at a location loc, returning 
     the result value b and the new location *)
  (* note: a change is that atoms may now be pinned to multiple 
           locations. So if the current location is multiple 
            locations when we reach an atom, we have to 
           execute all the instances of it. *)
  let rec eval : type x y. (x * y) pipe -> loc -> x list -> (loc * y list) 
    = fun pipe cur_loc xs ->
      match pipe with
      (* CHECK: atom should have |loc| = 1 and |xs| = 1*)
      | Atom(_, atom) -> cur_loc, [(atom.f (List.hd xs))]
      (* CHECK: |shard's input loc| = 1, and |xs| = 1 *)
      | Shard(_, _) -> 
        (* if we are sharding to multiple locations... then we actually pass on a list *)
        let x = List.hd xs in
        let pkt = fst x in
        let dsts = snd x in
        let dsts = Loc.flatten [dsts] in
        let pkts = List.map (fun _ -> pkt) dsts in
        Loc.Locs dsts, pkts
      (* CHECK: |move's input loc| = 1, and |xs| = 1 *)
      | Move(_, dst) -> dst, xs
      (* I think we handle locate and vector the same, because they're both composed of 
         lists of vectors, and the input location is supposed to be a subset of the 
         start locations of those vectors. *)
      | Locate(_, ps)
      | Vector(ps) -> 
        (* NEW CHECK: each element in a vector starts and ends at the same location *)        
        (* at this point, the locations represent which inner pipelines to run *)
        (* and xs should have 1 input for each pipeline *)
        let locs_to_run = Loc.flatten [cur_loc] in
        (* print_endline ("in VECTOR");
        print_endline ("inputs: "^(String.concat ", " (List.map (fun x -> "some x") xs)));
        print_endline ("locs to run: "^(Loc.loc_to_string (Loc.Locs locs_to_run))); *)
        if (List.length locs_to_run) <> (List.length xs) then failwith "runtime bug: vector pipeline has mismatched locations and inputs";
        (* now, we have to choose the right pipelines from ps to run *)
        let next_locs, ys = List.fold_left
          (fun (next_locs, ys) p -> 
            let (p_start, p_end, _) = inf_loc p in
            (* print_endline (Printf.sprintf "checking for p_start = %s in locs_to_run = %s" (Loc.loc_to_string p_start) (Loc.loc_to_string (Loc.Locs locs_to_run)));             *)
            if (Loc.is_subset p_start locs_to_run) then 
              let (next_loc, y) = eval p p_start [(List.nth  xs (List.length ys))] in
              (next_locs@[next_loc], ys@y)
            else (next_locs, ys))      
          ([], [])
          ps
        in
        (* print_endline ("output of pipe: "^(pipe_to_string pipe));
        print_endline ("next locs: "^(Loc.loc_to_string (Loc.Locs next_locs)));
        print_endline ("output: "^(String.concat ", " (List.map (fun y -> "some y") ys))); *)
        Loc.Locs next_locs, ys
      | Compose(p1, p2) -> 
        let cur_loc, ys = eval p1 cur_loc xs in
        let cur_loc, zs = eval p2 cur_loc ys in
        cur_loc, zs
  ;;


(* some helpers *)
let check_and_print pipe = 
  print_endline ("--- checking pipe locations --- ");
  let (pipe_start, pipe_end, pipe_checked) = inf_loc pipe in
  print_endline ("--- done --- ");
  print_endline ("pipe: "^(pipe_to_string pipe_checked));
  print_endline ("inferred start: " ^ (Loc.loc_to_string pipe_start));
  print_endline ("inferred end: " ^ (Loc.loc_to_string pipe_end));
  pipe_start, pipe_end, pipe_checked
;;

let run_from start pipe inp = 
  let end_loc, outputs = eval pipe start [inp] in
  let end_locs = Loc.flatten [end_loc] in
  (* want to return (loc, result) pairs *)
  if (List.length end_locs) <> (List.length outputs) then failwith "runtime bug: mismatched end locations and outputs";
  List.map2 (fun loc output -> (loc, output)) end_locs outputs
;;

let typecheck pipe = 
  let (pipe_start, pipe_end, pipe_checked) = inf_loc pipe in
  pipe_checked
;;

let exec pipe inp = 
  let pipe_start, _, pipe_checked = inf_loc pipe in
  run_from pipe_start pipe_checked inp
;;

end
(* open Pipe  *)
(* open Loc *)
let (@@@) = Pipe.(@@@);;
let (>>>) = Pipe.(>>>);;

(* test cases: 
   
  four pipelines that span switch -> NIC -> cpu cores 1 and 2
  each pipeline parses (on switch); selects processing core (on NIC); swaps source and dst addr (on one or more cores, in parallel)

  pipe1: cores 1 and 2 run "swap" with non-shared state. NIC selects one of the two cores to run on
  pipe2: cores 1 and 2 run "swap" with shared state. NIC selects one of the two cores to run on
  pipe3: cores 1 and 2 run "swap" with non-shared state. NIC selects both cores to run on
  pipe4: cores 1 and 2 run "swap" with shared state. NIC selects both cores to run on
*)


type raw_pkt = bytes
type parsed_pkt = {src : int; dst : int; payload : bytes}

let parsed_pkt_to_string pkt = 
  "src: "^(string_of_int pkt.src)^", dst: "^(string_of_int pkt.dst)^", payload: "^(Bytes.to_string pkt.payload)
;;

(* atoms *)
let parse_f bs = 
  (* 1. ensure length of bs is > 2
     2. first byte as src, 2nd byte as dst, rest as payload *)
  let src = int_of_char (Bytes.get bs 0) in
  let dst = int_of_char (Bytes.get bs 1) in
  let payload = Bytes.sub bs 2 ((Bytes.length bs) - 2) in
  {src = src; dst = dst; payload = payload}
;;

let swap_f pkt = 
  {src = pkt.dst; dst = pkt.src; payload = pkt.payload}
;;

let select_one_core pkt = 
  (pkt, Pipe.cores [1])
;;

let select_both_cores pkt = 
  (pkt, Pipe.cores [1; 2])
;;

let parse = Pipe.atom "parse" parse_f;;
let swap = Pipe.atom "swap" swap_f;;
let select_one_core = Pipe.atom "select_one_core" select_one_core;;
let select_both_cores = Pipe.atom "select_both_cores" select_both_cores;;

(* pipe1 *)
let pipe = 
  parse @@@ Pipe.switch
  >>> Pipe.move_to Pipe.nic 
  >>> select_one_core @@@ Pipe.nic
  >>> Pipe.shard_to (Pipe.cores [1; 2]) 
  >>> Pipe.parallel (Pipe.cores [1; 2]) swap
;;

let results = Pipe.exec pipe (Bytes.of_string "abc") ;;

print_endline ("parallel results: ");;
List.iter (fun (loc, output) -> 
  print_endline ("result: "^(parsed_pkt_to_string output)^" at "^(Pipe.loc_to_string loc))) results
;;

(* pipe2 *)
let pipe = 
  parse @@@ Pipe.switch
  >>> Pipe.move_to Pipe.nic 
  >>> select_one_core @@@ Pipe.nic
  >>> Pipe.shard_to (Pipe.cores [1; 2]) 
  >>> Pipe.shared (Pipe.cores [1; 2]) swap
;;

let results = Pipe.exec pipe (Bytes.of_string "abc") ;;
print_endline ("shared results: ");;

List.iter (fun (loc, output) -> 
  print_endline ("result: "^(parsed_pkt_to_string output)^" at "^(Pipe.loc_to_string loc))) results
;;

(* pipe3 *)
let pipe = 
  parse @@@ Pipe.switch
  >>> Pipe.move_to Pipe.nic 
  >>> select_both_cores @@@ Pipe.nic
  >>> Pipe.shard_to (Pipe.cores [1; 2]) 
  >>> Pipe.parallel (Pipe.cores [1; 2]) swap
;;
let results = Pipe.exec pipe (Bytes.of_string "abc") ;;
print_endline ("parallel replicated results: ");;
List.iter (fun (loc, output) -> 
  print_endline ("result: "^(parsed_pkt_to_string output)^" at "^(Pipe.loc_to_string loc))) results
;;

(* pipe4 *)
let pipe = 
  parse @@@ Pipe.switch
  >>> Pipe.move_to Pipe.nic 
  >>> select_both_cores @@@ Pipe.nic
  >>> Pipe.shard_to (Pipe.cores [1; 2]) 
  >>> Pipe.shared (Pipe.cores [1; 2]) swap
;;
let results = Pipe.exec pipe (Bytes.of_string "abc") ;;
print_endline ("shared replicated results: ");;
List.iter (fun (loc, output) -> 
  print_endline ("result: "^(parsed_pkt_to_string output)^" at "^(Pipe.loc_to_string loc))) results
;;
