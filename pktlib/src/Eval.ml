open Syntax

type 'x located = Loc.t * 'x
let rec eval : type x y. (_, x , y) annotated_pipe -> x located list -> (y located list) = 
  fun pipe located_xs ->
    match pipe with
    | Atom(_, atom) -> 
      List.map (fun (loc, x) -> (loc, atom#f x)) located_xs
    | Copy(_, n) -> 
      List.init n (fun _ -> List.hd located_xs)
    | Locate(_) -> located_xs
    | Share(_) -> located_xs    
    | Move(_, _) -> 
      List.map (fun (_, (x, dst)) -> (dst, (x))) located_xs
    | Sequence(_, p1, p2) -> 
      let located_ys = eval p1 located_xs in
      eval p2 located_ys
    | Parallel(_, p1, p2) -> 
      (* choose p1 if the location of x is in p1 *)
      let p1_starts, _, _ = LocAnalysis.infer_locations (Syntax.from_unit_pipe p1) in
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
    run_from Loc.Network (Syntax.to_unit_pipe pipe) inp
  ;;
