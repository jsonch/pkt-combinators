(* Modeling a combinator-based packet processing language in ocaml *)

(* an atom takes: 
  - a "constructor" that initializes some persistent/mutable state 
  - a function that takes a reference to the state and a "packet record" of some type, 
     and returns an optional packet record of a (possibly different) type *)
type ('state, 'x, 'y) atom = {
  constr : unit -> 'state ref;
  f : 'state ref -> 'x -> 'y option
}

(* schedulers are like atoms, except they also return an output port, which is an index into a vector of streams *)
type ('s, 'x, 'y) scheduler = {
  sched_constr : unit -> 's ref;
  sched_f : 's ref -> 'x -> ('y * int) option;
}

(* When you use pass atoms and schedulers to combinators, you get back 
   functions over streams. A stream is just a functional-programming 
   linked list, with some lazy evaluation that is an implementation detail *)
type 'a stream = Nil | Cons of 'a * 'a stream Lazy.t

(*** combinators ***)

(* 
  A combinator is just a function that either returns a stream processing function or 
  is a stream processing function. Combinators take atoms, schedulers, or stream processing 
  functions as arguments. Combinators so far:

  iter(atom) --  take an atom from x -> y option and return a stream function from stream<x> -> stream<y>
  shard(n, scheduler) -- take a scheduler from x -> (y, int) option and return a function from 
                         stream<x> -> list<stream<y>>, where the list has n streams
  parallel(n, stream_fcn) -- take (stream_fcn : stream<x> -> stream<y>), and 
                             return a list of stream functions (list<stream<x>> -> list<stream<y>>), 
                             where the list contains n stream functions and each function 
                             has its own copy of state.
  flatten (stream_list) -- take a list of streams and combine them together in some way to get a 
                           single stream.
  compose (f : stream_fcn) (g : stream_fcn) -- put two stream functions together: 
                                               call g on f's output. 
*)

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

(* shard -- split the output of a packetstream into a list of packetstreams, 
  using a scheduler to decide where each item in the input stream goes. *)
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

(* run n_pipes in parallel *)
let parallel n_pipes (pipe : (unit -> 'x stream -> 'y stream))  () = 
  let pipe_replicas = List.init n_pipes (fun i -> pipe ()) in
  fun (stream_vector : 'x stream list) ->
    if (n_pipes <> List.length stream_vector) then failwith "type error: for parallel, number of pipes must match number of input streams";
    (* run the pipeline on every stream in the vector *)
    List.mapi (fun i sv -> (List.nth pipe_replicas i) sv) stream_vector
;;

(* combine elements from multiple streams into 1 stream *)
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


type ('x, 'y) pipeline = unit -> 'x -> 'y



(* constructor for pipelines (just type for documentation / readability) *)
let make (p: unit -> 'x -> 'y) : ('x, 'y) pipeline = p;;
  

(* compose two pipes together *)
let compose 
  (f : ('x, 'y) pipeline) 
  (g : ('y, 'z) pipeline)
  : ('x, 'z) pipeline =
    fun () x' -> g () (f () x')
;;

let (>>>) a b = compose a b ;;

(* some helpers to make declaring atoms prettier *)
let no_constr = fun _ -> ref None;;
let create_int i = fun _ -> ref i;; 
let create_list l = fun _ -> ref l;;

(* some helpers for working with streams *)
let rec list_to_stream ls = match ls with 
  | [] -> Nil
  | l::ls' -> Cons(l, lazy (list_to_stream ls'))
;;
let rec stream_to_list ss = match ss with
  | Nil -> []
  | Cons(x, ss) -> x::(stream_to_list (Lazy.force ss))
;;


(*** driver functions  ***)
(* process a finite list, get back a list *)
let process_list name pipeline list = 
  stream_to_list (pipeline (list_to_stream list))
;;

(* process an infinite stream *)
let rec process_stream stream =
  match stream with
  | Nil -> ()
  | Cons (x, xs) ->
      (* process x *)
      process_stream (Lazy.force xs)
;;

(*** example programs ***)

(*  
  So far, the example programs written with this library have 3 parts:
  1. datatypes -- declaring the "structs" that 
     represent packets in different formats (e.g., unparsed_t, pkt_t below)
  2. atoms -- declaring the atoms that implement different pieces 
     of the program. E.g., parsing, deparsing, packet history annotation, 
     scheduling, port knocking. Atoms can use helper functions, too.
  3. pipelines -- putting the atoms together into a pipeline, using the 
     combinators.  
*)

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

let inputs = mk_test_pkts 20 0 100;;
let parallel_results = process_list "parallel_port_knock" (parallel_port_knock ()) inputs ;;
let serial_results = process_list "serial_port_knock" (serial_port_knock ()) inputs;;

print_endline "---- parallel results (packets that passed port knock fw) ----";;
newline_sep pkt_to_string parallel_results |> print_endline;;
print_endline "---- serial results (packets that passed port knock fw) ----";;
newline_sep pkt_to_string serial_results |> print_endline;;


let infinite_stream_test () = 
  (* test the streaming library with an infinite stream *)
  let rec infinite_input dport = 
    let dport = dport mod 255 in
    let pkt = mk_unparsed_t true dport dport in  
    Cons(pkt, lazy (infinite_input (dport + 1)))
  in

  let pipeline = (iter tcp_parser) >>> (iter (counter "element: ")) in

  print_endline ("--- starting infinite input test---");
  process_stream (pipeline () (infinite_input 0));
  ()
;;
(* warning: infinite stream test (should) run forever *)
(* infinite_stream_test () *)