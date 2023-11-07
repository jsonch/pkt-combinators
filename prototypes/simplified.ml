(* Simplify the AST primitives as much as possible *)
(* Just need to modularize this, then I think it will be good... *)

type ('x, 'y) atom = 
  { name : string; 
    f : 'x -> 'y;
  }

module Loc = struct

  type t = 
  | Network
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

  let compare loc1 loc2 = 
    match (loc1, loc2) with
    | (Switch, Switch) -> 0
    | (Switch, _) -> -1
    | (_, Switch) -> 1
    | (NIC, NIC) -> 0
    | (NIC, _) -> -1
    | (_, NIC) -> 1
    | (Network, Network) -> 0
    | (Network, _) -> -1
    | (_, Network) -> 1
    | (Core i, Core j) -> Int.compare i j
    | (Core _, _) -> -1
    | (_, Core _) -> 1
    | (Unbound i, Unbound j) -> Int.compare i j

  ;;
  let equal loc1 loc2 = 
    compare loc1 loc2 = 0
  ;;
  let rec to_string loc = 
    match loc with
    | Network -> "Network"
    | Switch -> "Switch"
    | NIC -> "NIC"
    | Core i -> "Core " ^ string_of_int i
    | Unbound i -> "Unbound " ^ string_of_int i
  ;;  
  let rec to_strings locs = 
    "["^(List.map to_string locs |> String.concat ", ")^"]"
end

module Pipe = struct 
  type loc = Loc.t
  type locset = loc list

  type ('a, 'x, 'y) annotated_pipe = 
    | Atom        : 'a * ('x, 'y) atom -> ('a, 'x , 'y) annotated_pipe (* construct an atomic pipeline *)  
    | Copy        : 'a * int -> ('a, 'x, 'x) annotated_pipe (* make n copies of the packet *)
    | Locate      : 'a * locset -> ('a, 'x, 'x) annotated_pipe (* start a pipeline at a location *)
    | Move        : 'a * locset -> ('a, ('x * loc) , 'x) annotated_pipe (* move each packet to a location *)
    | Share       : 'a * bool -> ('a, 'x, 'x) annotated_pipe (* toggle state sharing for subsequent pipeline *)
    | Sequence    : 'a * ('a, 'x , 'i) annotated_pipe * ('a, 'i , 'y) annotated_pipe -> ('a, 'x , 'y) annotated_pipe (* run two pipelines in sequence *)
    | Parallel    : 'a * ('a, 'x , 'y) annotated_pipe * ('a, 'x , 'y) annotated_pipe -> ('a, 'x , 'y) annotated_pipe (* run two pipelines in parallel *)
  ;;
  (* a pipe is just an annotated pipe annotated with units *)
  type ('x, 'y) pipe = (unit, 'x, 'y) annotated_pipe

  let atom name f : ('x, 'y) pipe = Atom((), {name; f})
  let copy n : ('x, 'x) pipe = Copy((), n)
  let move locs : ('x * loc, 'x) pipe = Move((), locs)
  let sequence p1 p2 : ('x, 'y) pipe = Sequence((), p1, p2)
  let (>>>) = sequence
  let parallel p1 p2 : ('x, 'y) pipe = Parallel((), p1, p2)
  let (|||) = parallel
  let shared : ('x, 'y) pipe = Share((), true)
  let unshared : ('x, 'y) pipe = Share((), false)
  let locate loc : ('x, 'x) pipe = Locate((), loc)

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

  let rec to_string : type x y. (x, y) pipe -> string = function
    | Atom (_, a) -> "Atom " ^ a.name
    | Copy (_, i) -> "Copy " ^ (string_of_int i)
    | Move (_, locs) -> "Move " ^ (Loc.to_strings locs)
    (* print operators for sequence and parallel, not constructors *)
    | Sequence (_, p1, p2) -> "(" ^ (to_string p1) ^ " >>> " ^ (to_string p2) ^ ")"
    | Parallel (_, p1, p2) -> "(" ^ (to_string p1) ^ " ||| " ^ (to_string p2) ^ ")"
    | Share (_, b) -> "Share " ^ (string_of_bool b)
    | Locate (_, locs) -> "Start " ^ (Loc.to_strings locs)
  ;;



  (* a pipeline that has start and end locations attached to it *)
  type ('x, 'y) located_pipe = ((locset * locset), 'x, 'y) annotated_pipe

  let startloc : type x y. (x, y) located_pipe -> locset = 
    function
    | Atom ((s, d), _) -> s
    | Copy ((s, d), _) -> s
    | Share ((s, d), _) -> s
    | Move ((s, d), _) -> s
    | Sequence ((s, d), _, _) -> s
    | Parallel ((s, d), _, _) -> s
    | Locate ((s, d), _) -> s
  ;;
  let endloc : type x y. (x, y) located_pipe -> locset = 
    function
    | Atom ((s, d), _) -> d
    | Copy ((s, d), _) -> d
    | Share ((s, d), _) -> d
    | Move ((s, d), _) -> d
    | Sequence ((s, d), _, _) -> d
    | Parallel ((s, d), _, _) -> d
    | Locate ((s, d), _) -> d
  ;;


  let rec pipe_to_located_pipe : type x y. (x, y) pipe -> (x, y) located_pipe = 
    fun pipe -> match pipe with
    | Atom (_, a) -> 
      let locs = [Loc.unbound ()] in
      Atom((locs, locs), a)
    | Copy (_, i) -> 
      let locs = [Loc.unbound ()] in
      Copy((locs, locs), i)
    | Share (_, b) ->
      let locs = [Loc.unbound ()] in
      Share((locs, locs), b)
    | Move (_, locs) ->
      let src_locs = [Loc.unbound ()] in
      let dst_locs = locs in
      Move((src_locs, dst_locs), locs)
    | Sequence(_, p1, p2) ->
      let p1' = pipe_to_located_pipe p1 in
      let p2' = pipe_to_located_pipe p2 in
      Sequence((startloc p1', endloc p2'), p1', p2')
    | Parallel(_, p1, p2) ->
      let p1' = pipe_to_located_pipe p1 in
      let p2' = pipe_to_located_pipe p2 in
      Parallel((startloc p1'@ startloc p2', endloc p1' @ endloc p2'), p1', p2')
    | Locate (_, locs) -> 
      Locate((locs, locs), locs)
  ;;

  let rec located_pipe_to_pipe : type x y. (x, y) located_pipe -> (x, y) pipe = 
    fun pipe -> match pipe with
    | Atom (_, a) -> Atom((), a)
    | Copy (_, i) -> Copy((), i)
    | Share (_, b) -> Share((), b)
    | Move (_, locs) -> Move((), locs)
    | Sequence (_, p1, p2) -> Sequence((), located_pipe_to_pipe p1, located_pipe_to_pipe p2)
    | Parallel (_, p1, p2) -> Parallel((), located_pipe_to_pipe p1, located_pipe_to_pipe p2)
    | Locate (_, locs) -> Locate((), locs)
  ;;

  (* location constraint analysis *)
  type loc_constraint = 
    | LocEq of loc * loc
    | LocNeq of loc * loc

  let loc_eq l1 l2 = LocEq(l1, l2)

  let locs_eq l1s l2s = 
    if (List.length l1s) <> (List.length l2s) then failwith "locs_eq: lists not same length";
    List.map2 loc_eq l1s l2s
  ;;

  (* for each location in l1, the same location does not appear in l2 *)
  let locs_no_overlap l1s l2s = 
    List.map (fun l1 -> List.map (fun l2 -> LocNeq(l1, l2)) l2s) l1s |> List.concat
  ;;

  let loc_constraint_to_string lc = 
    match lc with
      | LocEq (l1, l2) -> (Loc.to_string l1) ^ " = " ^ (Loc.to_string l2)
      | LocNeq (l1, l2) -> (Loc.to_string l1) ^ " != " ^ (Loc.to_string l2)
  ;;
  let loc_constraints_to_string lcs = 
    "["^(String.concat ", " (List.map loc_constraint_to_string lcs))^"]"
  ;;
  (* get all the constraints on locations *)
  let rec get_loc_constraints : type x y. (x, y) located_pipe -> loc_constraint list = 
    fun pipe -> match pipe with
    | Atom ((s, d), _)
    | Copy ((s, d), _)
    | Share ((s, d), _) -> locs_eq s d
    | Move ((s, d), locs) -> [] (*move can be from anywhere to anywhere*)
    | Sequence ((s, d), p1, p2) -> 
      let p1_constrs = get_loc_constraints p1 in
      let p2_constrs = get_loc_constraints p2 in
      let compose_constrs = locs_eq (endloc p1) (startloc p2) in
      (* no end constraints *)
      p1_constrs @ p2_constrs @ compose_constrs
    | Parallel ((s, d), p1, p2) -> 
      let p1_constrs = get_loc_constraints p1 in
      let p2_constrs = get_loc_constraints p2 in
      let compose_constrs = locs_no_overlap (startloc p1) (startloc p2) in
      p1_constrs @ p2_constrs @ compose_constrs
    | Locate((s, d), locs) -> locs_eq s locs
  ;;
  let get_loc_constraints lpipe = 
    (* get constraints, add constraint that program begins in network *)
    let constrs = get_loc_constraints lpipe in
    (* let start_loc = startloc lpipe in
    let constrs = locs_eq start_loc [Loc.Network] @ constrs in *)
    constrs
  ;;

  type equiv_loc_set =  loc list;;

  let rec unique_list xs = 
    match xs with 
    | [] -> []
    | hd::tl -> 
      if List.mem hd tl then unique_list tl
      else hd :: unique_list tl
  ;;
  
  let union set1 set2 = unique_list (set1 @ set2) ;;
  let find equiv_sets loc = List.find_opt (fun set -> List.mem loc set) equiv_sets ;;
  
(* update a the list of location equivalence sets according to a list of constraints *)
let rec update_equivs equiv_sets (loc_constraints : loc_constraint list) : equiv_loc_set list =
  let res = match loc_constraints with
  | [] -> equiv_sets
  | LocNeq _ :: tl -> update_equivs equiv_sets tl
  | LocEq(loc1, loc2) :: tl -> (
    let l1_set = find equiv_sets loc1 in
    let l2_set = find equiv_sets loc2 in
    let equiv_sets = match l1_set, l2_set with
      | None, None ->unique_list [loc1; loc2] :: equiv_sets
      | Some(s1), None -> (loc2 :: s1) :: List.filter ((!=) s1) equiv_sets
      | None, Some(s2) -> (loc1 :: s2) :: List.filter ((!=) s2) equiv_sets
      | Some(s1), Some(s2) -> union s1 s2 :: List.filter ((!=) s1) (List.filter ((!=) s2) equiv_sets)
    in  
    update_equivs equiv_sets tl)   
  in 
  res 
;;

(* check each location set, making sure that no inequality constraint is violated *)
let check_not_eq_constrs equiv_sets not_eq_constrs = 
  List.for_all (fun set -> 
    List.for_all (fun lc -> match lc with
      | LocNeq (l1, l2) -> not (List.mem l1 set && List.mem l2 set)
      | _ -> true (* skip other constraints*)
    ) not_eq_constrs
  ) equiv_sets
;;

(* for each equiv set, find the core(s) and the unknown(s) *)
let rec get_mappings equiv_sets =
  match equiv_sets with 
  | [] -> []
  | equiv_locs::tl ->
    (* find the concrete locations *)
    let concrete_locs = List.filter (fun loc -> match loc with | Loc.Unbound _ -> false | _ -> true) equiv_locs in
    (* find the unknown(s) *)
    let unknowns = List.filter (fun loc -> match loc with | Loc.Unbound _ -> true | _ -> false) equiv_locs in
    (* if there's no concrete location, use the first core as the root *)
    if ((List.length concrete_locs) = 0) then (
      let core = List.hd unknowns in
      List.map (fun unknown -> (unknown, core)) unknowns @ get_mappings tl
    )
    else if ((List.length concrete_locs) > 1) 
      then failwith (Printf.sprintf "unify_locations: more than one core in equiv set (%s)" (Loc.to_strings equiv_locs))
    else 
      let core = List.hd concrete_locs in
      List.map (fun unknown -> (unknown, core)) unknowns @ get_mappings tl
;;

let replace_loc pipe loc_mappings loc = 
  match loc with
  | Loc.Unbound _ -> (
    match List.assoc_opt loc loc_mappings with 
    | None -> failwith (Printf.sprintf "replace_loc: unbound loc not in mapping. Pipe: %s, loc: %s" (to_string pipe) (Loc.to_string loc))
    | Some(loc') -> loc'
  )
  | _ -> loc
;;

let rec update_locs : type x y. (loc * loc) list -> (x, y) located_pipe -> (x, y) located_pipe =
  fun mappings pipe -> 
    let rlocs = List.map (replace_loc (located_pipe_to_pipe pipe) mappings) in
    match pipe with
    | Atom((s, d), a) -> Atom((rlocs s, rlocs d), a)
    | Copy((s, d), i) -> Copy((rlocs s, rlocs d), i)
    | Share((s, d), b) -> Share((rlocs s, rlocs d), b)
    | Move((s, d), locs) -> Move((rlocs s, rlocs d), locs)
    | Sequence((s, d), p1, p2) -> Sequence((rlocs s, rlocs d), update_locs mappings p1, update_locs mappings p2)
    | Parallel((s, d), p1, p2) -> Parallel((rlocs s, rlocs d), update_locs mappings p1, update_locs mappings p2)
    | Locate((s, d), locs) -> Locate((rlocs s, rlocs d), locs)
;;

let infer_locations pipe = 
  (* convert into a located pipe *)
  let located_pipe = pipe_to_located_pipe pipe in
  (* get constraints on locations *)
  let loc_constraints = get_loc_constraints located_pipe in
  (* get sets of equiv locations *)
  let equiv_sets = update_equivs [] loc_constraints in
  (* check not equal constraints *)
  let not_eq_constrs_pass = check_not_eq_constrs equiv_sets loc_constraints in
  if not_eq_constrs_pass = false then failwith "infer_locations: not equal constraints violated";
  (* compute mapping from unbound loc -> concrete loc *)
  let mapping = get_mappings equiv_sets in 
  (* update the located pipe with the mapping *)
  let located_pipe' = update_locs mapping located_pipe in
  (* return pipe start, end, and pipe *)
  (startloc located_pipe', endloc located_pipe', located_pipe')
;;

(*** evaluation ***)

type 'x located = Loc.t * 'x
let rec eval : type x y. (x , y) pipe -> x located list -> (y located list) = 
  fun pipe located_xs ->
    match pipe with
    | Atom(_, atom) -> 
      List.map (fun (loc, x) -> (loc, atom.f x)) located_xs
    | Copy(l, n) -> 
      List.init n (fun _ -> List.hd located_xs)
    | Locate(_) -> located_xs
    | Share(_) -> located_xs    
    | Move(_, _) -> 
      List.map (fun (loc, (x, dst)) -> (dst, (x))) located_xs
    | Sequence(_, p1, p2) -> 
      let located_ys = eval p1 located_xs in
      eval p2 located_ys
    | Parallel(_, p1, p2) -> 
      (* choose p1 if the location of x is in p1 *)
      let p1_starts, _, _ = infer_locations p1 in
      List.fold_left 
      (fun ys (loc, x) -> 
        let pipe = if List.mem loc p1_starts then p1 else p2 in
        ys@(eval pipe [(loc, x)]))
      []
      located_xs
  ;;

  let run_from start pipe inp = 
    let outputs = eval pipe  [start, inp] in
    outputs
  ;;

  let exec pipe inp = 
    run_from Loc.Network pipe inp
  ;;


end

open Loc
open Pipe

type raw_pkt = bytes
type parsed_pkt = {src : int; dst : int; payload : bytes}

let parsed_pkt_to_string pkt = 
  "{src: "^(string_of_int pkt.src)^", dst: "^(string_of_int pkt.dst)^", payload: "^(Bytes.to_string pkt.payload)^"}"
;;
let parse = atom "parse"
  (fun bs -> 
     let src = int_of_char (Bytes.get bs 0) in
     let dst = int_of_char (Bytes.get bs 1) in
     let payload = Bytes.sub bs 2 ((Bytes.length bs) - 2) in
     {src = src; dst = dst; payload = payload})   
;;

let round_robin_2_ctr = ref 0 ;; 
let round_robin_2 = atom "round_robin_2" 
  (fun pkt -> 
    let ctr = !(round_robin_2_ctr) in
    round_robin_2_ctr := ctr + 1;
    let next_core = if ctr mod 2 = 0 then core 1 else core 2 in
    (pkt, next_core))
;;
let swap = atom "swap"
  (fun pkt -> 
    {src = pkt.dst; dst = pkt.src; payload = pkt.payload})
;;

let test = locate [nic] >>> (move [core 1; core 2] >>> (swap ||| swap));;

let start, end_, test' = infer_locations test;;
print_endline ("test starts at: "^(Loc.to_strings start));;
print_endline ("test ends at: "^(Loc.to_strings end_));;

let start loc = locate [loc]
let shard = move
let move l = 
  let cmove = atom "const_move" (fun x -> (x, l)) in
  cmove >>> move [l]
;;
let cores cs = List.map core cs



let test_pipe name pipe item_printer = 

  print_endline ("--- "^name^" ---");
  let start, end_, _ = infer_locations pipe in
  print_endline ("pipe spans from "^(Loc.to_strings start)^" to "^(Loc.to_strings end_));

  let results = Pipe.exec pipe (Bytes.of_string "abc") in
  List.iter (fun (loc, output) -> 
    print_endline ((item_printer output)^" from "^(Loc.to_string loc))) results
;;



let pipe1 = 
  start switch >>> parse
  >>> move nic 
  >>> copy 2 
  >>> round_robin_2 
  >>> shard (cores [1; 2]) >>> parallel_shared (cores [1; 2]) swap
;;

test_pipe "pipe1" pipe1 parsed_pkt_to_string;;
