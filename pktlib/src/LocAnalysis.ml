(* analyze pipe locations *)

open Syntax

(* a pipeline that has start and end locations attached to it *)
type ('x, 'y) located_pipe = ((locset * locset), 'x, 'y) annotated_pipe

(* a set of locations *)

(* locations of located pipes *)
let locs : type x y. (x, y) located_pipe -> locset * locset = 
  function
  | Atom ((s, d), _) -> (s, d)
  | Copy ((s, d), _) -> (s, d)
  | Share ((s, d), _) -> (s, d)
  | Move ((s, d), _) -> (s, d)
  | Sequence ((s, d), _, _) -> (s, d)
  | Parallel ((s, d), _, _) -> (s, d)
  | Locate ((s, d), _) -> (s, d)
;;

let startloc : type x y. (x, y) located_pipe -> locset = 
  fun lpipe -> fst (locs lpipe)
;;
let endloc : type x y. (x, y) located_pipe -> locset = 
  fun lpipe -> snd (locs lpipe)
;;

let rec pipe_to_located_pipe : type x y. (_, x, y) annotated_pipe -> (x, y) located_pipe = 
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

let rec located_pipe_to_pipe : type x y. (x, y) located_pipe -> (_, x, y) annotated_pipe = 
  fun pipe -> match pipe with
  | Atom (_, a) -> Atom((), a)
  | Copy (_, i) -> Copy((), i)
  | Share (_, b) -> Share((), b)
  | Move (_, locs) -> Move((), locs)
  | Sequence (_, p1, p2) -> Sequence((), located_pipe_to_pipe p1, located_pipe_to_pipe p2)
  | Parallel (_, p1, p2) -> Parallel((), located_pipe_to_pipe p1, located_pipe_to_pipe p2)
  | Locate (_, locs) -> Locate((), locs)
;;

(* a constraint on two locations *)
type loc_constraint = 
| LocEq of loc * loc
| LocNeq of loc * loc

let loc_eq l1 l2 = LocEq(l1, l2)

let locs_eq msg l1s l2s = 
  if (List.length l1s) <> (List.length l2s) then 
    failwith
      @@"locations are different cardinalities: "
      ^(List.length l1s |> string_of_int)^" vs " ^ (List.length l2s |> string_of_int)
      ^" in: "^(msg);
    
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
    | Share ((s, d), _) -> locs_eq (Syntax.to_string pipe) s d
    | Move (_) -> [] (*move can be from anywhere to anywhere*)
    | Sequence (_, p1, p2) -> 
      let p1_constrs = get_loc_constraints p1 in
      let p2_constrs = get_loc_constraints p2 in
      let msg = Printf.sprintf "Compose(\n\t%s,\n\t%s)" (Syntax.to_string p1) (Syntax.to_string p2) in
      let compose_constrs = locs_eq msg (endloc p1) (startloc p2) in
      (* no end constraints *)
      p1_constrs @ p2_constrs @ compose_constrs
    | Parallel (_, p1, p2) -> 
      let p1_constrs = get_loc_constraints p1 in
      let p2_constrs = get_loc_constraints p2 in
      let compose_constrs = locs_no_overlap (startloc p1) (startloc p2) in
      p1_constrs @ p2_constrs @ compose_constrs
    | Locate((s, _), locs) -> locs_eq (Syntax.to_string pipe) s locs
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

let replace_loc (pipe : (_, 'x, 'y) annotated_pipe) loc_mappings loc = 
  match loc with
  | Loc.Unbound _ -> (
    match List.assoc_opt loc loc_mappings with 
    | None -> failwith (Printf.sprintf "replace_loc: unbound loc not in mapping. Pipe: %s, loc: %s" (Syntax.to_string pipe) (Loc.to_string loc))
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

let infer_locations (pipe : ('x, 'y) pipe) = 
  let pipe = Syntax.to_unit_pipe pipe in
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
