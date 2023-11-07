(* 
  combinators with location annotations. 
  Key ideas: 
    - Pipelines are packet processing functions composed with combinators.
    - Combinators let the user turn functions into pipelines, 
      sequentially compose pipelines, and specify how pipelines should be 
      distributed across locations in the system (e.g., switch, nic, cpu cores).
    - The raw input and output types of a pipeline (e.g., a raw_packet -> parsed_packet pipeline)
      is checked by ocaml's type system.    
    - The locations of pipeline components are checked by our own analysis pass.

  TODO: 
    - add state back into atoms
*)


module type PipeSig = sig
  type loc
  type locset
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
  val parallel : locset -> ('x * 'y) pipe -> ('x * 'y) pipe
  val shared : locset -> ('x *'y) pipe -> ('x * 'y) pipe

  (* Builtin pipelines *)
  (* given a target location, construct a pipeline that moves 
     each packet from the current location to the target location  *)
  val move_to : loc -> ('x * 'x) pipe
  val start_at : loc -> ('x * 'x) pipe

  (* Given a list of possible target locations, construct a pipeline 
     that takes a stream of "location-annotated packets" ('x * loc)
     and moves each packet from its current location to the 
     locations specified by its annotation. 
     The locations specified by an annotation must be a subset of 
     the target locations. *)
  val shard_to : locset -> (('x * locset) * 'x) pipe

  (* operators *)
  (* ">>>" is compose *)
  val (>>>) : ('x * 'y) pipe -> ('y * 'z) pipe -> ('x * 'z) pipe
  (* "@@@" is locate *)
  val (@@@) : ('x * 'y) pipe -> loc -> ('x * 'y) pipe

  (* pipe analysis and execution *)
  val typecheck : ('x * 'y) pipe -> ('x * 'y) pipe
  val exec : ('x * 'y) pipe -> 'x -> (loc * 'y) list

  (* a few location helpers *)
  val loc_to_string : loc -> string
  val switch : loc
  val nic : loc
  val cores : int list -> locset

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

  let switch = Switch 
  let nic = NIC
  let core i = Core(i)
  (* a never-used unbound location *)
  let ct = ref 0
  let unbound () = 
    ct := !ct + 1;
    Unbound !ct
  ;;

  (* refresh a location if it is unbound *)
  let refresh loc = match loc with
    | Unbound _ -> unbound ()
    | _ -> loc
  ;;
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
  ;;
  let equal loc1 loc2 = 
    compare loc1 loc2 = 0
  ;;

  let rec loc_to_string loc = 
    match loc with
    | Switch -> "Switch"
    | NIC -> "NIC"
    | Core i -> "Core " ^ string_of_int i
    | Unbound i -> "Unbound " ^ string_of_int i
  ;;  
end


module Pipe : PipeSig = struct 
  type loc = Loc.t
  module LocSet = Set.Make(Loc)

  type locset = LocSet.t
  let cores core_ids = List.map (Loc.core) core_ids |> LocSet.of_list
  let nic = Loc.nic
  let switch = Loc.switch

  (* singleton locsets *)
  let to_sset = LocSet.singleton
  let from_sset ls = 
    if (LocSet.cardinal ls) <> 1 then failwith "expected singleton locset";
    LocSet.elements ls |> List.hd
  ;;
  (* locset constructors *)
  let loc_to_string = Loc.loc_to_string
  let locset_refresh = LocSet.map Loc.refresh
  let locset_to_string ls = 
    "{" ^ (String.concat ", " (List.map loc_to_string (LocSet.elements ls))) ^ "}"
  ;;

  type ('x, 'y) atom = 
    { name : string; 
      f : 'x -> 'y;
    }
  
  type _ pipe =  
    | Atom : loc * ('x, 'y) atom -> ('x * 'y) pipe
    | Locate : locset * ('x * 'y) pipe list -> ('x * 'y) pipe (* bind a pipe to one or more locations *)
    | Move   : loc * loc -> ('x * 'x) pipe (* move from one location to another *)
    | Shard  : loc * locset -> (('x * locset) * 'x) pipe 
    | Vector : ('x * 'y) pipe list -> ('x * 'y) pipe
    | Compose : ('x * 'y) pipe * ('y * 'z) pipe -> ('x * 'z) pipe
  ;;

  let rec pipe_to_string : type a b. (a * b) pipe -> string = function
    | Atom(l, a) -> "Atom(" ^ (Loc.loc_to_string l) ^ ", " ^ a.name ^ ")"
    | Locate(ls, ps) -> "Locate(" ^ (locset_to_string ls) ^ ", " ^ (pipes_to_string ps) ^ ")"
    | Move(l1, l2) -> "Move(" ^ (Loc.loc_to_string l1) ^ ", " ^ (Loc.loc_to_string l2) ^ ")"
    | Shard(l, ls) -> "Shard(" ^ (Loc.loc_to_string l) ^ ", " ^ (locset_to_string ls) ^ ")"
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
      | Locate(ls, ps) -> Locate(locset_refresh ls, List.map refresh_locs ps)
      | Move(l1, l2) -> Move(Loc.refresh l1, Loc.refresh l2)
      | Shard(l1, l2) -> Shard(Loc.refresh l1, locset_refresh l2)
      | Vector(ps) -> Vector(List.map refresh_locs ps)
      | Compose(p1, p2) -> Compose(refresh_locs p1, refresh_locs p2)
    in
    refresh_locs pipe
  ;;  

  (* public combinator wrappers *)
  (* user-constructed atoms have unbound locations *)
  let atom name f = Atom (Loc.unbound (), {name = name; f = f})
  (* users can only annotate a pipeline with a single location -- 
     use parallel or shared for task-parallel pipelines, 
     or move/shard for stage-parallel pipelines *)
  let locate (loc : loc) p = Locate (to_sset loc, [p])
    
  (* loc, List.map (fun _ -> refresh_locs (p)) (LocSet.elements loc)) *)
  let (@@@) p l = locate l p

  (* users only specify the destination of a move -- 
     source is inferred *)
  let move_to dst = Move((Loc.unbound ()), dst)
  let start_at = move_to

  (* users only specify the possible destinations of a shard -- 
     the source is inferred *)
  let shard_to dsts = Shard (Loc.unbound (), dsts)
  let compose p1 p2 = Compose (p1, p2)
  let (>>>) = compose

  (* parallel is implemented as a vector of pipelines that is each located separately *)
  let parallel locset pipe = 
    let pipes = List.map (fun loc -> Locate((LocSet.singleton loc), [(refresh_locs pipe)])) (LocSet.elements locset) in
    let res = Vector pipes in
    res
  ;;
  (* shared is implemented as a vector of pipelines that are located at the same time, 
     i.e., its just a wrapper for the full "locate" *)
  let shared locset pipe = 
    let locate loc p = Locate (loc, List.map (fun _ -> refresh_locs (p)) (LocSet.elements loc)) in
    let res = locate locset pipe in
    res
  ;;

  (*** location inference ***)

  let rec unify_locs l1 l2 = 
    match (l1, l2) with
    | (Loc.Unbound i, Loc.Unbound j) -> Loc.Unbound (min i j)
    | (Loc.Unbound i, _) -> l2
    | (_, Loc.Unbound i) -> l1
    | _, _ -> if Loc.equal l1 l2 then l1 else failwith "unification of locations failed"
  ;;

  (* unification of two ordered locsets *)
  let rec unify_locsets l1 l2 = 
    let l1 = LocSet.elements l1 in
    let l2 = LocSet.elements l2 in
    (* try to unify the elements, print message if error happens *)
    let rec unify_loc_lists l1 l2 = 
      match (l1, l2) with
      | ([], []) -> []
      | (hd1::tl1, hd2::tl2) -> 
        let hd = unify_locs hd1 hd2 in
        let tl = unify_loc_lists tl1 tl2 in
        hd::tl
      | _ -> failwith "unification of location sets failed"
    in
    LocSet.of_list (unify_loc_lists l1 l2)
  ;;

  let replace_loc oldloc newloc curloc = 
    if Loc.equal curloc oldloc then newloc else curloc
  ;;
  let replace_loc_in_locset oldloc newloc locset = 
    LocSet.map (replace_loc oldloc newloc) locset
  ;;

  (* replace every instance of location oldloc 
     with location newloc, for use after unification *)
  let rec replace_loc_in_pipe : type a b. loc -> loc -> (a * b) pipe -> (a * b) pipe = 
    fun oldloc newloc -> 
      let rloc = replace_loc oldloc newloc in
      let rlocset = replace_loc_in_locset oldloc newloc in
      let rpipe : type a b. (a * b) pipe -> (a * b) pipe = fun p -> replace_loc_in_pipe oldloc newloc p in
      let rpipes = List.map rpipe in
      function
      | Atom(l, a) -> Atom(rloc l, a)
      | Locate(ls, ps) -> Locate(rlocset ls, rpipes ps)
      | Move(l1, l2) -> Move(rloc l1, rloc l2)
      | Shard(l1, l2) -> Shard(rloc l1, rlocset l2)
      | Vector(ps) -> Vector(rpipes ps)
      | Compose(p1, p2) -> Compose(rpipe p1, rpipe p2)
  ;;

  let replace_loc = replace_loc_in_pipe;;

  let rec replace_locs : type a b. locset -> locset -> (a * b) pipe -> (a * b) pipe = 
    fun oldlocs newlocs pipe -> 
      match (LocSet.elements oldlocs, LocSet.elements newlocs) with
        | ([], []) -> pipe
        | (hd1::tl1, hd2::tl2) -> 
          let pipe = replace_loc_in_pipe hd1 hd2 pipe in
          replace_locs (LocSet.of_list tl1) (LocSet.of_list tl2) pipe
        | _ -> failwith "replace_locs_in_pipe: mismatched locsets"
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
  let rec inf_loc : type a b. (a * b) pipe -> (locset * locset * (a * b) pipe)
    = fun pipe -> 
    (* use n_tabs to add spaces to debug string *)
    let tab () = String.make (!n_tabs * 2) ' ' in
    n_tabs := !n_tabs + 1;
    dprint_endline (tab () ^ "[inf start] "^(pipe_to_string pipe));
    let res = match pipe with 
    | Atom (l, a) -> 
      (to_sset l, to_sset l, pipe)
    | Locate(ls, ps) -> 
      if (LocSet.cardinal ls) <> (List.length ps) then failwith "location inference failed in a locate -- mismatch between locations and pipes";
      (* infer the locations of the inner pipes *)      
      let located_ps = List.map2 
        (fun l p -> 
          let (p_start, p_end, p) = inf_loc p in
          (* inferred locations for inner pipes should be singletons *)
          let p_start, p_end = from_sset p_start, from_sset p_end in
          let (p_start, p_end, p) = unify_and_recheck l p_start p in
          (* still singletons... *)
          let p_start, p_end = from_sset p_start, from_sset p_end in
          (p_start, p_end, p))
        (LocSet.elements ls) ps 
      in
      let starts, ends, ps = List.fold_left (fun (starts, ends, ps) (s, e, p) -> 
        (LocSet.add s starts, LocSet.add e ends, ps@[p])) (LocSet.empty, LocSet.empty, []) located_ps
      in
      (* the located pipe goes from the union of the starts of the inner pipes to the union of the ends of the inner pipes *)
      starts, ends, Locate(starts, ps)
    | Shard(src, dsts) -> (to_sset src, dsts, pipe)
    | Move(src, dst) -> (to_sset src, to_sset dst, pipe)
    | Vector (ps) -> 
      (* infer start and end locations from inner pipes *)
      let starts, ends, ps = inf_locs ps in 
      (* no unification, so no recheck needed *)
      (starts, ends, Vector ps)
    | Compose(p1, p2) -> 
      (* unify end of p1 with start of p2 *)
      let (ps1, pe1, p1) = inf_loc p1 in
      let (ps2, pe2, p2) = inf_loc p2 in
      (* pe1 (pipe 1 end locs) and ps2 (pipe 2 start locs) 
         could be loc sets with multiple elements  *)
      let p_middle_new = try 
        unify_locsets pe1 ps2 
        with _ -> 
          print_endline ("location inference failure trying to unify:\n"^(locset_to_string pe1)^"\nand\n"^(locset_to_string ps2));
          print_endline ("pipes:\n"^(pipe_to_string p1)^"\nand\n"^(pipe_to_string p2));
          failwith "location inference failed in a compose -- mismatch between locations of inner pipes"      
      in
      let p1 = replace_locs pe1 p_middle_new p1 in
      let p2 = replace_locs ps2 p_middle_new p2 in
      (* now, we have to recheck the locations of p1 and p2 *)
      let (ps1, pe1, p1) = inf_loc p1 in
      let (ps2, pe2, p2) = inf_loc p2 in
      (ps1, pe2, Compose(p1, p2))
    in 
    let start, fin, pipe' = res in
    dprint_endline (tab () ^ "[inf end] "^(pipe_to_string pipe') ^ " : ("^(locset_to_string start)^", "^(locset_to_string fin)^")");
    n_tabs := !n_tabs - 1;
    res

  and inf_locs : type a b. (a * b) pipe list -> (locset * locset * (a * b) pipe list)
    = fun pipes -> 
    let located_ps = List.map inf_loc pipes in
    let starts, ends, ps = List.fold_left (fun (starts, ends, ps) (s, e, p) -> 
      (LocSet.union s starts, LocSet.union e ends, ps@[p])) (LocSet.empty, LocSet.empty, []) located_ps
    in
    starts, ends, ps

  and unify_and_recheck 
  : type a b. loc -> loc -> (a * b) pipe -> (locset * locset * (a * b) pipe)
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
  let rec eval : type x y. (x * y) pipe -> locset -> x list -> (locset * y list) 
    = fun pipe cur_loc xs ->
      match pipe with
      | Atom(_, atom) -> cur_loc, [(atom.f (List.hd xs))]
      (* TODO CHECK: |shard's input loc| = 1, and |xs| = 1 *)
      | Shard(_, _) -> 
        (* if we are sharding to multiple locations... then we actually pass on a list *)
        let x = List.hd xs in
        let pkt = fst x in
        let dsts = snd x in
        let pkts = List.init (LocSet.cardinal dsts) (fun _ -> pkt) in
        dsts, pkts
      (* TODO CHECK: |move's input loc| = 1, and |xs| = 1 *)
      | Move(_, dst) -> to_sset dst, xs
      | Locate(_, ps)
      | Vector(ps) -> 
        (* NEW CHECK: each element in a vector starts and ends at the same location *)        
        (* at this point, the locations represent which inner pipelines to run *)
        (* and xs should have 1 input for each pipeline *)
        let locs_to_run = LocSet.elements cur_loc in
        if (List.length locs_to_run) <> (List.length xs) then failwith "runtime bug: vector pipeline has mismatched locations and inputs";
        (* now, we have to choose the right pipelines from ps to run *)
        let next_locs, ys = List.fold_left
          (fun (next_locs, ys) p -> 
            let (p_start, p_end, _) = inf_loc p in
            let p_start = from_sset p_start in
            if (LocSet.mem p_start cur_loc) then 
              let (next_loc, y) = eval p (to_sset p_start) [(List.nth  xs (List.length ys))] in
              let next_loc = from_sset next_loc in
              (LocSet.add next_loc next_locs, ys@y)
            else (next_locs, ys))
          (LocSet.empty, [])
          ps
        in
        (* print_endline ("output of pipe: "^(pipe_to_string pipe));
        print_endline ("next locs: "^(Loc.loc_to_string (Loc.Locs next_locs)));
        print_endline ("output: "^(String.concat ", " (List.map (fun y -> "some y") ys))); *)
        next_locs, ys
      | Compose(p1, p2) -> 
        let cur_loc, ys = eval p1 cur_loc xs in
        let cur_loc, zs = eval p2 cur_loc ys in
        cur_loc, zs
  ;;


(* some helpers *)

let run_from start pipe inp = 
  let end_locs, outputs = eval pipe start [inp] in
  if (LocSet.cardinal end_locs) <> (List.length outputs) then failwith "runtime bug: mismatched end locations and outputs";
  List.map2 (fun loc output -> (loc, output)) (LocSet.elements end_locs) outputs
;;

let typecheck pipe = 
  let (pipe_start, pipe_end, pipe_checked) = inf_loc pipe in
  print_endline ("pipeline = "^(pipe_to_string pipe_checked)^"\nlocation span = ("^(locset_to_string pipe_start)^", "^(locset_to_string pipe_end)^")");
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
let parse = Pipe.atom "parse"
  (fun bs -> 
  (* 1. ensure length of bs is > 2
     2. first byte as src, 2nd byte as dst, rest as payload *)
     let src = int_of_char (Bytes.get bs 0) in
     let dst = int_of_char (Bytes.get bs 1) in
     let payload = Bytes.sub bs 2 ((Bytes.length bs) - 2) in
     {src = src; dst = dst; payload = payload})   
;;

let select_one_core = Pipe.atom "select_one_core" 
  (fun pkt -> (pkt, Pipe.cores [1]))
;;
let select_both_cores = Pipe.atom "select_both_cores" 
  (fun pkt -> (pkt, Pipe.cores [1; 2]))
;;
let swap = Pipe.atom "swap"
  (fun pkt -> 
    {src = pkt.dst; dst = pkt.src; payload = pkt.payload})
;;

(* pipe1 *)
let pipe = 
  parse @@@ Pipe.switch
  >>> Pipe.move_to Pipe.nic 
  >>> select_one_core
  >>> Pipe.shard_to (Pipe.cores [1; 2]) 
  >>> Pipe.parallel (Pipe.cores [1; 2]) swap
;;

let _ = Pipe.typecheck pipe ;;
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

let _ = Pipe.typecheck pipe ;;
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

let _ = Pipe.typecheck pipe ;;
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
let _ = Pipe.typecheck pipe ;;
let results = Pipe.exec pipe (Bytes.of_string "abc") ;;
print_endline ("shared replicated results: ");;
List.iter (fun (loc, output) -> 
  print_endline ("result: "^(parsed_pkt_to_string output)^" at "^(Pipe.loc_to_string loc))) results
;;

(* currently, this fails -- there's no "merge/join/unshard" operator *)
let pipe = 
  parse @@@ Pipe.switch
  >>> Pipe.move_to Pipe.nic 
  >>> select_both_cores @@@ Pipe.nic
  >>> Pipe.shard_to (Pipe.cores [1; 2]) 
  >>> Pipe.shared (Pipe.cores [1; 2]) (swap >>> Pipe.move_to Pipe.switch)
;;
let _ = Pipe.typecheck pipe;;
let results = Pipe.exec pipe (Bytes.of_string "abc") ;;
(* print_endline ("back to switch results: ");; *)
