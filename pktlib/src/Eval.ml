open Syntax
open Pipe

type 'x located = Loc.t * 'x


let rec setup : type i. (_, i) annotated_pipe -> (_, i) instantiated_pipe = 
  fun pipe -> 
    match pipe with
    | Atom (_, a, args) -> Atom ((), (instantiate a (a.state_init ())), args)
    | Let (_, a, args, ret, p) -> Let ((), (instantiate a (a.state_init ())), args, ret, setup p)
    | Copy (_, _) -> failwith "Unimplemented"
    | Locate (_, locs, p) -> Locate((), locs, setup(p))
    | Move _ -> failwith "Unimplemented"
    | Share _ -> failwith "Ambiguous, unimplemented, use the shared atom generator."
    | Sequence (_, p1, p2) -> Sequence ((), (setup p1), (setup p2))
    | Parallel (_, p1, p2) -> Parallel ((), (setup p1), (setup p2))

(*Only call this with an instantiated pipe!*)
(*Also totally ignores location stuff...*)
let eval : type i. (_, i) instantiated_pipe -> i -> i -> i = 
  fun pipe pkt_len input_packet ->
    let rec eval_acc pipe (ctx : i ArgMap.t) = 
      match pipe with 
      | Atom (_, a, args) -> a.f () args ctx
      | Let (_, a, args, ret, p) -> let ctx' = ArgMap.add ret (a.f () args ctx) ctx in eval_acc p ctx'
      | Copy (_, _) -> failwith "Unimplemented"
      | Locate (_, _, p) -> eval_acc p ctx
      | Move _ -> failwith "Unimplemented"
      | Share _ -> failwith "Ambiguous, unimplemented, use the shared atom generator."
      | Sequence (_, p1, p2) -> let ctx' = ArgMap.add "packet" (eval_acc p1 ctx) ctx in eval_acc p2 ctx'
      | Parallel (_, p1, p2) -> let _ = eval_acc p1 ctx in eval_acc p2 ctx
    in
    let ctx = ArgMap.empty in
    let ctx = ArgMap.add "packet" input_packet ctx in
    let ctx = ArgMap.add "pkt_len" pkt_len ctx in
    eval_acc pipe ctx


  let run_from start pipe inp = 
    let outputs = eval pipe  [start, inp] in
    outputs
  ;;
  let exec pipe inp = 
    run_from Loc.Network (Syntax.to_unit_pipe pipe) inp
  ;;
