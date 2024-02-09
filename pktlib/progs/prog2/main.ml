[@@@ocaml.warning "-33"]
[@@@ocaml.warning "-32"]
[@@@ocaml.warning "-34"]
open Pktlib
open Loc
open Pipe
open LocAnalysis
open Eval
open CTypes
open Foreign
open Ffi_bindings

(* helper to test a pipeline *)
let test_pipe name pipe item_printer = 

  print_endline ("--- "^name^" ---");
  let start, end_, _ = infer_locations pipe in
  print_endline ("pipe spans from "^(Loc.to_strings start)^" to "^(Loc.to_strings end_));

  let results = exec pipe (Bytes.of_string "abc") in
  List.iter (fun (loc, output) -> 
    print_endline ((item_printer output)^" from "^(Loc.to_string loc))) results
;;


let aparse = atom "parse" (fun (s, pkt, m) -> unit)

(*Open the c library*)
let libadd = Dl.dlopen ~flags:[Dl.RTLD_LAZY] ~filename:"portknock"
(* packet types *)
type unparsed_pkt = bytes
type parsed_pkt = {src : int; dst : int; payload : bytes}
let pipe1 = 
  start switch >>> aparse
;;

(*
(* packet print helpers *)
let parsed_pkt_to_string pkt = 
  "{src: "^(string_of_int pkt.src)^", dst: "^(string_of_int pkt.dst)^", payload: "^(Bytes.to_string pkt.payload)^"}"
;;


(* parse on switch, move to nic, duplicate packet and round robin to cores 1 and 2, 
   run swap_addr on both cores with shared state *)
let pipe1 = 
  start switch >>> parse
  >>> const_move nic 
  >>> copy 2 >>> round_robin_2 >>> shard (cores [1; 2]) 
  >>> parallel_shared (cores [1; 2]) swap_addrs 
;;

(* this pipe will fail because we don't run anything on core2 *)
let pipe2 = 
  start switch >>> parse
  >>> const_move nic 
  >>> copy 2 >>> round_robin_2 >>> shard (cores [1; 2]) 
  >>> (at (core 1) swap_addrs) 
;;

(* this pipe runs swap_addrs on core1 and random_addrs on core2 *)
let pipe3 = 
  start switch >>> parse
  >>> const_move nic 
  >>> copy 2 >>> round_robin_2 >>> shard (cores [1; 2]) 
  >>> ((at (core 1) swap_addrs) ||| (at (core 2) random_addrs))
;;
*)

let main () = 
  test_pipe "pipe1" pipe1 parsed_pkt_to_string;
  (*try 
    test_pipe "pipe2" pipe2 parsed_pkt_to_string
  with
    | Failure(msg) -> 
      print_endline "pipe2 failed";
      print_endline msg;
  test_pipe "pipe3" pipe3 parsed_pkt_to_string;*)
  ()
;;



main ();;