(* Modeling a combinator-based packet processing language in ocaml *)
(* A modular implementation that hides "state" from users 
   and also a module to demonstrate how to do analysis over 
   streams by re-defining pipelines and combinator 
   implementations in a cost analysis module. 
   (look at "PipeCostSig" around line170) *)
(* helper modules for Atoms and Streams *)
module Atom = struct 
  type ('state, 'x, 'y) atom = {
    constr : unit -> 'state ref;
    f : 'state ref -> 'x -> 'y option
  }

  type ('state, 'x, 'y) scheduler = {
    sched_constr : unit -> 'state ref;
    sched_f : 'state ref -> 'x -> ('y * int) option;
  }

  (* some helpers to make declaring atoms prettier *)
  let no_constr = fun _ -> ref None;;
  let create_int i = fun _ -> ref i;; 
  let create_list l = fun _ -> ref l;;

end

module Stream = struct 
  type 'a stream = Nil | Cons of 'a * 'a stream Lazy.t

  (* some helpers for working with streams *)
  let rec list_to_stream ls = match ls with 
  | [] -> Nil
  | l::ls' -> Cons(l, lazy (list_to_stream ls'))
  ;;
  let rec stream_to_list ss = match ss with
  | Nil -> []
  | Cons(x, ss) -> x::(stream_to_list (Lazy.force ss))
  ;;
end

(* the base pipeline module signature *)
module type PIPE = sig
  open Atom 
  open Stream
  type ('x, 'y) pipeline
  (* combinators *)
  (* pipeline composition *)
  val compose : ('x, 'y) pipeline -> ('y, 'z) pipeline -> ('x, 'z) pipeline
  val (>>>) : ('x, 'y) pipeline -> ('y, 'z) pipeline -> ('x, 'z) pipeline
  (* pipeline constructors *)
  val iter : ('s, 'x, 'y) atom -> ('x stream, 'y stream) pipeline
  val shard : int -> ('s, 'x, 'y) scheduler -> ('x stream, ('y stream) list) pipeline
  val parallel : int -> ('x stream, 'y stream) pipeline -> ('x stream list, 'y stream list) pipeline
  val flatten : ('x stream list, 'x stream) pipeline
end

(* signature of a module that evaluates a pipeline *)
module type EVALPIPE = sig
  open Stream
  include PIPE
  type ('x, 'y) compiled_pipeline
  val compile : ('x, 'y) pipeline -> ('x, 'y) compiled_pipeline 
  val map_list : ('x stream, 'y stream) compiled_pipeline -> 'x list -> 'y list
  (* val iter_stream  *)
  val run : ('x stream, 'y stream) compiled_pipeline 
    -> (unit -> 'x stream) 
    -> ('y stream -> unit) 
    -> unit
end

(* an implementation of a pipeline evaluator *)
module EvalPipe : EVALPIPE = struct
  open Atom
  open Stream
  type ('x, 'y) pipeline = unit -> 'x -> 'y
  type ('x, 'y) compiled_pipeline = 'x -> 'y
  let map_list (p:('x stream, 'y stream) compiled_pipeline) list = 
    stream_to_list (p (list_to_stream list))
  ;;  

  let run 
    (p:('x stream, 'y stream) compiled_pipeline) 
    (stream_producer : unit -> 'x stream)
    (stream_consumer  : 'y stream -> unit) =
      let inp_stream = stream_producer () in
      stream_consumer (p inp_stream)
  ;;

  let compile (p:('x, 'y) pipeline) = p ();;
  let iter (atom : ('s, 'x, 'y) atom) () : 'x stream -> 'y stream = 
    (* first, create the state *)
    let state = atom.constr () in
    (* then, create the function that processes the stream over the state*)
    let rec process_stream (inp : 'x stream) : 'y stream = 
      match inp with
        | Nil -> Nil
        | Cons (x, xs) -> 
          let output = atom.f state x in (* *)
          match output with
          | None -> process_stream (Lazy.force xs)
          | Some(output) -> Cons (output, lazy (process_stream (Lazy.force xs)))
    in
    process_stream 
  ;;
  let shard n (scheduler : ('s, 'x, 'y) scheduler)  () : ('x stream -> ('y stream) list) = 
    (* first, create the state *)
    let state = scheduler.sched_constr () in
    let rec process inp = 
      match inp with 
        | Nil -> List.init n (fun _ -> Nil)
        | Cons(x, xs) ->    
          (* 1. run the scheduler function and get a result. *)
          let output = scheduler.sched_f state x in
          let x_out, out_port = match output with 
            | None -> failwith "not implemented"
            | Some(x, port) -> (x, port)
          in
          (* compute the rest of the stream -- lazily *)
          let rest_of_stream = lazy (process (Lazy.force xs)) in
          List.init n
            (fun i -> 
              if (i = out_port) then 
                Cons(x_out, lazy (List.nth (Lazy.force rest_of_stream) i))
              else
                (List.nth (Lazy.force rest_of_stream) i))
    in
    process
  ;;

  let parallel n_pipes (pipe : ('x stream, 'y stream) pipeline) () = 
    let pipe_replicas = List.init n_pipes (fun i -> pipe ()) in
    fun (stream_vector : 'x stream list) ->
      if (n_pipes <> List.length stream_vector) then failwith "type error: for parallel, number of pipes must match number of input streams";
      (* run the pipeline on every stream in the vector *)
      List.mapi (fun i sv -> (List.nth pipe_replicas i) sv) stream_vector
  ;;

  let rec flatten () (streams : 'x stream list) : 'x stream =   
    (* take one element from each stream *)
    let rec take_one (streams : 'x stream list) : 'x stream = 
      match streams with
      | [] -> Nil
      | stream :: streams' -> (
        match stream with 
        | Cons(x, xs) -> (
          let rest = lazy (take_one (streams'@[Lazy.force xs])) in
          Cons(x, rest)
        )
        | Nil -> take_one streams'
      )
    in
    take_one streams
  ;;

  let compose 
  (f : ('x, 'y) pipeline) 
  (g : ('y, 'z) pipeline)
  : ('x, 'z) pipeline =
    fun () x' -> g () (f () x')
  ;;

  let (>>>) a b = compose a b ;;

  (* constructor for pipelines (just type for documentation / readability) *)
  (* let make (p: 'x -> 'y) : ('x, 'y) pipeline = (fun _ -> p);; *)

end

(* signature of a module that computes cost *)
module type PipeCostSig = sig
  include PIPE
  val cost : _ pipeline -> float  
end

(* a module that computes pipeline cost *)
module PipeCost : PipeCostSig = struct 
  open Atom
  open Stream 
  type ('x, 'y) pipeline = unit -> float
  let cost (p:('x, 'y) pipeline) = 
    p ()
  let iter atom () = 1.0
  let shard _ _ () = 0.0
  let parallel n_cores inner_p () = 
    ((cost inner_p) /. (float n_cores))
  let flatten () = 0.0
  let compose p1 p2 () = 
    ((cost p1) +. (cost p2))
  let (>>>) = compose
end


(*** example programs ***)
open Atom
open Stream
open EvalPipe

let n_cores = 2;;


(*** packet / event types ***)
type unparsed_t = 
{
  buf : bytes;
}
type tcp_pkt_t = 
{
  sport : int;
  dport : int;
  payload : bytes;
}

type 'pkt annotated_pkt_t = 
{
  pkt_histories : 'pkt list; (* past n_cores packets *)
  pkt : 'pkt;
}

(*** atoms ***)
(* parse an "unparsed_t" into a "parsed_t" *)
let tcp_parser = {
  constr = no_constr;
  f = (fun _ pkt -> (* the first argument is an underscore because we don't use state in this atom *)
  (* assume first byte determines if its tcp *)
    if ((Bytes.get_uint8 pkt.buf 0) <> 0) then 
      (
        let sport = Bytes.get_uint8 pkt.buf 1 in
        let dport = Bytes.get_uint8 pkt.buf 2 in
        let payload = Bytes.sub pkt.buf 3 (Bytes.length pkt.buf - 3) in
        Some({sport; dport; payload})
      )
    else
      None
  );
}
;;

(* deparse a packet back to an unparsed *)
let tcp_deparser = {
  constr = no_constr;
  f = (fun _ pkt -> 
    let buf = Bytes.create (3 + (Bytes.length pkt.payload)) in
    Bytes.set_uint8 buf 0 1;
    Bytes.set_uint8 buf 1 pkt.sport;
    Bytes.set_uint8 buf 2 pkt.dport;
    Bytes.blit pkt.payload 0 buf 3 (Bytes.length pkt.payload);
    Some({buf})    
    );
}
;;  

let counter label = {
  constr = create_int 0;
  f =  (fun state (pkt : 'x) -> 
    state := !state + 1;
    print_endline@@label^":"^(string_of_int !state);
    Some(pkt));  
};;

let printer label = {
    constr = create_int 0;
    f = (fun _ pkt -> 
      print_endline 
        (Printf.sprintf "[%s] sport=%i, dport=%i" label pkt.sport pkt.dport);
      Some(pkt)      
    );
};;


let annotator = {
    constr = create_list [];
    f = (fun histories pkt -> 
      let out_histories = 
        if n_cores = 1 then [] 
        else 
          if ((List.length (!histories)) = (n_cores - 1)) then (
            let out_h = !histories in
            histories := (List.tl !histories)@[pkt];
            out_h
            )
          else (
            let out_h = !histories in
            histories := !histories@[pkt];
            out_h        
          )
      in
      Some({pkt_histories = out_histories; pkt}));
}
;;

let noop_annotator = {
  constr = no_constr;
  f = (fun _ pkt -> Some({pkt_histories = []; pkt;}));
}
;;

let rr_scheduler = {
  sched_constr = create_int 0;
  sched_f = (fun state (pkt : 'x) -> 
    let port = !state in
    state := (port + 1) mod n_cores;
    Some(pkt, port)      
    )
}
;;

let port_knock = 
  (* the state transition function could just as well be 
     declared outside of "port_knock" -- its not part of 
     the atom. And the port knock sequence of 
     [100; 101; 102] is hard-coded. *)
  let update_state state pkt = 
    match (!state, pkt.dport) with
    | (0, 100) -> state := 1;
    | (1, 101) -> state := 2;
    | (2, 102) -> state := 3;
    | (3, _  ) -> ()
    | (_, _  ) -> state := 0;
  in  
  {
    constr = create_int 0;
    f = (fun state pkt -> 
      for i = 0 to List.length (pkt.pkt_histories) - 1 do
        update_state state (List.nth pkt.pkt_histories i)
      done;            
      update_state state pkt.pkt;
      if (!state = 3) then Some(pkt.pkt) else None    
    );
}
;;



(* programs *)
let parallel_port_knock = 
  (iter tcp_parser)
  >>> (iter annotator)
  (* move to main cores *)
  >>> (shard n_cores rr_scheduler)
  >>> (parallel n_cores (iter port_knock))
  (* flatten just for debugging *)
  >>> flatten
;;

let serial_port_knock = 
  (iter tcp_parser)
  >>> (iter noop_annotator)
  >>> (iter port_knock)
;;



(*** test harness ***)
let pkt_to_string pkt = 
  Printf.sprintf "sport=%i dport=%i" pkt.sport pkt.dport
;;
let comma_sep stringerizer values = String.concat "," (List.map stringerizer values)   
;;
let newline_sep str_fn vs = String.concat "\n" (List.map str_fn vs)
let pkts_to_string pkts = newline_sep pkt_to_string pkts
;;
let mk_unparsed_t is_tcp sport dport = 
  if (sport > 255 || dport > 255) then 
    failwith "ports are encoded as bytes. Sorry!";
  let buf = Bytes.create 4 in
  if (is_tcp) then 
    Bytes.set_uint8 buf 0 1
  else
    Bytes.set_uint8 buf 0 1;
  Bytes.set_uint8 buf 1 sport;
  Bytes.set_uint8 buf 2 dport;
  {buf}
;;

let mk_test_pkts n_pkts sport_start dport_start = 
  (* make a list, then cast it to a stream *)
  List.init 
    n_pkts
    (fun i -> 
      mk_unparsed_t true (i+sport_start) (i+dport_start))
;;  

let inputs = mk_test_pkts 20 0 100;;

let parallel_prog = compile parallel_port_knock;;

let parallel_results = EvalPipe.map_list parallel_prog inputs ;;
let serial_results = EvalPipe.map_list (compile serial_port_knock) inputs;;

print_endline "---- parallel results (packets that passed port knock fw) ----";;
newline_sep pkt_to_string parallel_results |> print_endline;;
print_endline "---- serial results (packets that passed port knock fw) ----";;
newline_sep pkt_to_string serial_results |> print_endline;;

let rec infinite_input dport = 
  let dport = dport mod 255 in
  let pkt = mk_unparsed_t true dport dport in  
  Cons(pkt, lazy (infinite_input (dport + 1)))
;;

let infinite_stream_test () = 
  (* test the streaming library with an infinite stream *)
  let rec input_pkt_stream dport = 
    let dport = dport mod 255 in
    let pkt = mk_unparsed_t true dport dport in  
    Cons(pkt, lazy (infinite_input (dport + 1)))
  in
  let rec consume_infinite_stream s = 
    match s with 
    | Nil -> ()
    | Cons(x, xs) -> 
      consume_infinite_stream (Lazy.force xs)
  in
  let pipe = (iter tcp_parser) >>> (iter (counter "element: ")) in
  let compiled_pipe = compile pipe in
  print_endline ("--- starting infinite input test---");
  run compiled_pipe ((fun _ -> input_pkt_stream 0)) (consume_infinite_stream);
  ()
;;

(* let _ = infinite_stream_test ();; *)
(* analyze pipeline cost by opening PipeCost instead of PipeEval *)
open PipeCost
let serial_port_knock = 
  (iter tcp_parser)
  >>> (iter noop_annotator)
  >>> (iter port_knock)
;;
let parallel_port_knock = 
  (iter tcp_parser)
  >>> (iter annotator)
  (* move to main cores *)
  >>> (shard n_cores rr_scheduler)
  >>> (parallel n_cores (iter port_knock))
  (* flatten just for debugging *)
  >>> flatten
;;

print_endline ("----- analyzing pipeline cost -----");;
let serial_cost = cost serial_port_knock;;
let parallel_cost = cost parallel_port_knock;;
print_endline ("serial version cost: "^((string_of_float serial_cost)));;
print_endline ("parallel version cost: "^((string_of_float parallel_cost)));;