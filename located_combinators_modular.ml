(* Modular location-based pipeline combinators. 


  Does not include state. *)

module type PipeSig = sig
  type loc
  type locset
  type ('x, 'y) atom
  type _ pipe

  (* locations *)
  val switch : loc
  val nic : loc
  val cores : int list -> locset (* a set of cores identified by ints *)

  (* pipeline combinators *)

  (* construct an atomic pipe *)
  val atom : string -> ('x -> 'y) -> ('x * 'y) pipe (* name * atom -> "atomic" pipeline *)
  (* compose two pipelines together *)
  val move : loc -> ('x * 'x) pipe (* move processing from one location to another. *)
  val start : loc -> ('x * 'x) pipe (* a "start" just tells you where to start the pipe *)
  val shard : locset -> (('x * locset) * 'x) pipe (* a shard is a builtin pipe that copies each input to a subset of outputs *)
  val compose : ('x * 'y) pipe -> ('y * 'z) pipe -> ('x * 'z) pipe
  val (>>>) : ('x * 'y) pipe -> ('y * 'z) pipe -> ('x * 'z) pipe (* the operator for compose *)

  (* construct input-parallel pipelines that runs concurrently at multiple 
     locations, with each pipeline using either local memory (parallel) 
     or shared memory (shared). *)
  val parallel_unshared : locset -> ('x * 'y) pipe -> ('x * 'y) pipe
  val parallel_shared : locset -> ('x * 'y) pipe -> ('x * 'y) pipe

  (* pipe analysis and execution *)
  val check_locations : ('x * 'y) pipe -> locset * ('x * 'y) pipe
  val exec : ('x * 'y) pipe -> 'x -> (loc * 'y) list

  (* a few location helpers *)
  val loc_to_string : loc -> string
 
end


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

let dprint_endline = print_endline
let dprint_endline _ = ()


module Pipe : PipeSig = struct 
  type loc = Loc.t
  module LocSet = Set.Make(Loc)
  type locset = LocSet.t
  type ('x, 'y) atom = 
  { name : string; 
    f : 'x -> 'y;
  }

  type _ pipe = 
    | Atom : loc * ('x, 'y) atom -> ('x * 'y) pipe (* construct an atomic pipeline *)  
    | Move   : loc * loc  -> ('x * 'x) pipe (* move processing from one location to another. *)
    | Compose : ('x * 'i) pipe * ('i * 'y) pipe -> ('x * 'y) pipe
    (* construct task-parallel pipelines, bool indicates whether 
      state is shared (true) or not (false) *)
    | Parallel : (bool * ((loc * ('x * 'y) pipe)) list) -> ('x * 'y) pipe
    (* move from a single-threaded pipeline at loc to a 
      subset of the parallel pipeline distributed across locset.
      Each input is annotated with the locations that it goes to. *)
    | Fork  : loc * locset -> (('x * locset) * 'x) pipe
  ;;  

  let switch = Loc.switch
  let nic = Loc.nic
  let cores core_ids = List.map (Loc.core) core_ids |> LocSet.of_list





  (* singleton locsets *)
  let to_sset = LocSet.singleton
  let from_sset ls = 
    if (LocSet.cardinal ls) <> 1 then failwith "expected singleton locset";
    LocSet.elements ls |> List.hd
  ;;
  let all_unbound locset = 
    LocSet.for_all (fun loc -> match loc with Loc.Unbound _ -> true | _ -> false) locset
  ;;
  let loc_to_string = Loc.loc_to_string
  let locset_refresh = LocSet.map Loc.refresh
  let locset_to_string ls = 
    "{" ^ (String.concat ", " (List.map loc_to_string (LocSet.elements ls))) ^ "}"
  ;;


let refresh_locs pipe = 
  let rec refresh_locs : type a b. (a * b) pipe -> (a * b) pipe = function
    | Atom(l, a) -> Atom(Loc.refresh l, a)
    | Move(l1, l2) -> Move(Loc.refresh l1, Loc.refresh l2)
    | Compose(p1, p2) -> Compose(refresh_locs p1, refresh_locs p2)
    | Parallel(b, loc_to_pipe) -> 
      Parallel(
        b, 
        List.map (fun (loc, pipe) -> Loc.refresh loc, refresh_locs pipe) loc_to_pipe
      )
    | Fork(l, ls) -> Fork(Loc.refresh l, locset_refresh ls)
  in
  refresh_locs pipe
;;

  (* user-constructed atoms have unbound locations *)
  let atom name f = Atom (Loc.unbound (), {name = name; f = f})

  (* users only specify the destination of a move -- 
     source is inferred *)
  let move dst = Move((Loc.unbound ()), dst)
  (* a "start" just tells you where to start the pipe *)
  let start loc = Move(loc, loc)

  (* a shard is just a fork from an unspecified location *)
  let shard dsts = Fork (Loc.unbound (), dsts)

  (* locate a pipe to a location *)
  let compose p1 p2 = Compose (p1, p2)
  let (>>>) = compose
  
  let parallel_shared locset pipe = 
    Parallel(true, 
      List.map (fun loc -> loc, refresh_locs pipe) (LocSet.elements locset)
    )
  ;;
  let parallel_unshared locset pipe = 
    Parallel(false, 
      List.map (fun loc -> loc, refresh_locs pipe) (LocSet.elements locset)
    )
  ;;

  let rec pipe_to_string : type a b. (a * b) pipe -> string = function
    | Atom(l, a) -> "Atom(" ^ (Loc.loc_to_string l) ^ ", " ^ a.name ^ ")"
    | Move(l1, l2) -> "Move(" ^ (Loc.loc_to_string l1) ^ ", " ^ (Loc.loc_to_string l2) ^ ")"
    | Compose(p1, p2) -> "Compose(" ^ (pipe_to_string p1) ^ ", " ^ (pipe_to_string p2) ^ ")"
    | Parallel(b, locs_to_pipe) -> "Parallel(" ^ (string_of_bool b) ^ ", " ^ (pipe_to_string (List.hd locs_to_pipe |> snd)) ^ ")"
    | Fork(l, ls) -> "Fork(" ^ (Loc.loc_to_string l) ^ ", " ^ (locset_to_string ls) ^ ")"
  ;;  


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
    function
    | Atom(l, a) -> Atom(rloc l, a)
    | Compose(p1, p2) -> Compose(rpipe p1, rpipe p2)
    | Move(l1, l2) -> Move(rloc l1, rloc l2)
    | Parallel(b, locs_to_pipes) -> 
      let locs_to_pipes = List.map
        (fun (loc, p) ->     
          rloc loc, rpipe p
          )
        locs_to_pipes
      in
      Parallel(b, locs_to_pipes)
    | Fork(l, ls) -> Fork(rloc l, rlocset ls)
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

  let n_tabs = ref 0

  let rec inf_loc : type a b. (a * b) pipe -> (locset * locset * (a * b) pipe)
    = fun pipe -> 
    (* use n_tabs to add spaces to debug string *)
    let tab () = String.make (!n_tabs * 2) ' ' in
    n_tabs := !n_tabs + 1;
    dprint_endline (tab () ^ "[inf start] "^(pipe_to_string pipe));
    let res = match pipe with 
    | Atom(l, a) -> (to_sset l, to_sset l, pipe)
    | Compose(p1, p2) -> 
      let (l1, l2, p1) = inf_loc p1 in
      let (l3, l4, p2) = inf_loc p2 in
      let l2 = unify_locsets l2 l3 in
      let p1 = replace_locs l2 l3 p1 in
      let p2 = replace_locs l2 l3 p2 in
      (l1, l4, Compose(p1, p2))
    | Move(l1, l2) -> 
      let l1 = to_sset l1 in
      let l2 = to_sset l2 in
      (l1, l2, pipe)
    | Parallel(is_shared, locs_to_pipes) -> 
      let (starts, ends, locs_to_pipes) = List.fold_left
        (fun (starts, ends, locs_to_pipes) (loc, p) ->  
          let (ss, es, p) = inf_loc p in
          let s = from_sset ss in
          let s' = unify_locs s loc in
          let p = replace_loc_in_pipe s s' p in
          let (ss, es, p) = inf_loc p in
          let s = from_sset ss in
          let e = from_sset es in
          starts@[s], ends@[e], locs_to_pipes@[s, p])
        ([], [], [])
        locs_to_pipes
      in
      let starts = LocSet.of_list starts in
      let ends = LocSet.of_list ends in
      (starts, ends, Parallel(is_shared, locs_to_pipes))
    | Fork(l, ls) -> 
      let l = to_sset l in
      (l, ls, pipe)
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
  ;;
  let check_locations pipe = 
    let (pipe_start, pipe_end, pipe_checked) = inf_loc pipe in
    pipe_start, pipe_checked
    (* print_endline ("pipeline = "^(pipe_to_string pipe_checked)^"\nlocation span = ("^(locset_to_string pipe_start)^", "^(locset_to_string pipe_end)^")"); *)
    (* pipe_checked *)
  ;;
  
  let rec eval : type x y. (x * y) pipe -> locset -> x list -> (locset * y list) = 
    fun pipe cur_loc xs ->
      match pipe with
      | Move(src, dst) -> LocSet.singleton dst, xs
      | Compose (p1, p2) -> let locset1, ys1 = eval p1 cur_loc xs in
                            let locset2, ys2 = eval p2 locset1 ys1 in
                            locset2, ys2
      | Parallel (_, loc_to_pipe) ->
        let locs = LocSet.elements cur_loc in
        let located_xs = List.combine locs xs in
        let located_ys = List.map 
          (fun (loc, x) -> let locset, y = eval (List.assoc loc loc_to_pipe) (LocSet.singleton loc) [x] in
                            locset, y
          ) located_xs 
        in
        let locsets, ys = List.split located_ys in
        List.fold_left LocSet.union LocSet.empty locsets, (List.flatten ys)
      | Fork (_, _) -> 
        let (x, dst_locset) = List.hd xs in
        dst_locset, List.init (LocSet.cardinal dst_locset) (fun _ -> x)
      | Atom(_, atom) -> cur_loc, [(atom.f (List.hd xs))]
  ;;

  let run_from start pipe inp = 
    let end_locs, outputs = eval pipe start [inp] in
    if (LocSet.cardinal end_locs) = 1 then 
      List.map (fun output -> (from_sset end_locs, output)) outputs
    else(
      if (LocSet.cardinal end_locs) <> (List.length outputs) then failwith "runtime bug: mismatched end locations and outputs";
      List.map2 (fun loc output -> (loc, output)) (LocSet.elements end_locs) outputs
    )
  ;;
  
  let exec pipe inp = 
    let pipe_start, _, pipe_checked = inf_loc pipe in
    run_from pipe_start pipe_checked inp
  ;;
end



(**** examples ****)
open Pipe


let test_pipe name pipe item_printer = 

  print_endline ("--- "^name^" ---");
  let start_locs, pipe = check_locations pipe in

  let results = Pipe.exec pipe (Bytes.of_string "abc") in
  List.iter (fun (loc, output) -> 
    print_endline ((item_printer output)^" from "^(Pipe.loc_to_string loc))) results
;;


(* packet types *)
type raw_pkt = bytes
type parsed_pkt = {src : int; dst : int; payload : bytes}

let parsed_pkt_to_string pkt = 
  "{src: "^(string_of_int pkt.src)^", dst: "^(string_of_int pkt.dst)^", payload: "^(Bytes.to_string pkt.payload)^"}"
;;

(* atoms *)
let parse = atom "parse"
  (fun bs -> 
     let src = int_of_char (Bytes.get bs 0) in
     let dst = int_of_char (Bytes.get bs 1) in
     let payload = Bytes.sub bs 2 ((Bytes.length bs) - 2) in
     {src = src; dst = dst; payload = payload})   
;;

let select_one_core = atom "select_one_core" 
  (fun pkt -> (pkt, cores [1]))
;;
let select_both_cores = atom "select_both_cores" 
  (fun pkt -> (pkt, cores [1; 2]))
;;
let swap = atom "swap"
  (fun pkt -> 
    {src = pkt.dst; dst = pkt.src; payload = pkt.payload})
;;

(* pipe1: cores 1 and 2 run "swap" with non-shared state. 
   NIC selects one of the two cores to run on. *)
let pipe1 = 
  start switch >>> parse
  >>> move nic >>> select_one_core 
  >>> shard (cores [1; 2]) >>> parallel_unshared (cores [1; 2]) swap
;;
(* pipe2: same as pipe1, but with shared state *)
let pipe2 = 
  start switch >>> parse
  >>> move nic >>> select_one_core 
  >>> shard (cores [1; 2]) >>> parallel_unshared (cores [1; 2]) swap
;;
(* pipe3: non-shared state, but now both cores process every packet *)
let pipe3 = 
  start switch >>> parse
  >>> move nic >>> select_both_cores 
  >>> shard (cores [1; 2]) >>> parallel_unshared (cores [1; 2]) swap
;;
(* pipe4: shared state with both cores processing every packet *)
let pipe4 = 
  start switch >>> parse
  >>> move nic >>> select_both_cores 
  >>> shard (cores [1; 2]) >>> parallel_shared (cores [1; 2]) swap
;;


test_pipe "pipe1" pipe1 parsed_pkt_to_string;;
test_pipe "pipe2" pipe2 parsed_pkt_to_string;;
test_pipe "pipe3" pipe3 parsed_pkt_to_string;;
test_pipe "pipe4" pipe4 parsed_pkt_to_string;;

exit 0;;