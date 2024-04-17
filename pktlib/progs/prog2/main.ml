open Ctypes
open Foreign
open Pktlib.Pipe
open Ffi_bindings
open Pktlib.Eval
(*Open the c library*)
type packet = unit ptr;;
let packet = ptr void;;
type pkt_len = unit ptr;;
let pkt_len = ptr void;;
type flow_key_t = unit ptr;;
let flow_key = ptr void;;
type update_result = unit ptr;;
let update_result = ptr void;;

let libknock = Dl.dlopen ~flags:[Dl.RTLD_LAZY] ~filename:"portknock"
let parse = foreign ~from:libknock "parse" (void @-> pkt_len @-> packet @-> returning flow_key)
let update_state_init = foreign ~from:libknock "update_state_init" (void @-> returning state)
let update =  foreign ~from:libknock "update" (state @-> flow_key @-> returning update_result)

let action = foreign ~from:libknock "action" (void @-> update_result @-> pkt_len @-> packet @-> returning update_result)
let handler = (ptr void @-> ptr void @-> returning (ptr void))
let test_func = foreign ~from:libknock "test_func" (int @-> int @-> returning int)
let run =
  foreign "run"
    (int @-> ptr string @-> funptr handler @-> returning int)
(*let get_handle = foreign ~from:libknock "get_pcap_handle" (Ctypes.string @-> returning pcap_handle)
let load_next_pcap_pkt = foreign ~from:libknock "load_next_pcap_pkt" (pcap_handle @-> pcap_pkthdr @-> returning (ptr char))
*)
module StringMap = Map.Make(String);;

(*OCaml wrappers for how to call the above functions using the labels
   I can't figure out how to do this generally...*)
let get_arguments arguments arg_map = List.map (fun arg -> match StringMap.find_opt arg arg_map with
      | Some(value) -> value
      | None -> failwith ("Missing argument: " ^ arg)) arguments



(*Pipe1*)
(*action*)
let aaction = atom "action" 4 (fun _ -> ()) (fun s args arg_map -> (let args = get_arguments args arg_map in action s (List.nth args 0) (List.nth args 1) (List.nth args 2))) 
(*action(update_result, pkt_len, packet)*)
let paction = atom_pipe aaction ["update_result"; "pkt_len"; "packet"]

(*update*)
let aupdate = atom "update" 2 update_state_init (fun s args arg_map -> (let args = get_arguments args arg_map in update s (List.nth args 0)))
(*update(flow_key)*)
let pupdate = atom_pipe aupdate ["flow_key"]
(*let update_result = update(flow_key) in action(update_result, pkt_len, packet)*)
let p_combined = let_pipe "update_result" pupdate paction

(*parse*)
let aparse = atom "parse" 3 (fun _ -> ()) (fun s args arg_map -> (let args = get_arguments args arg_map in parse s (List.nth args 0) (List.nth args 1)))
(*parse(pkt_len, packet)*)
let pparse = atom_pipe aparse  ["pkt_len"; "packet"]
(*let flow_key = parse(pkt_len, packet) in [...above...]*)
let p_combined_2 = let_pipe "flow_key" p_combined



(*Pipe2*)
(* Shared update state
pipe update_s := shared(update);
pipe p1 := (let flow_key = parse(pkt_len, packet) in
  let update_result = update_s(flow_key) in
    action(update_result, pkt_len, packet));
p2 := (p1 @ c1) ||| (p1 @ c2)

  Is different from 
p1 := (let flow_key = aparse(pkt_len, packet) in 
  let update_result = update(flow_key) in
    aaction(update_result, pkt_len, packet));
p2 := (p1 @ c1) ||| (p1 @ c2)
*)

(*shared_atom a : unit -> a*)
let shared_update = shared_atom aupdate
let p_shared_update_1 = let_pipe "update_result" (shared_update ()) aaction
let p_shared_update_2 = let_pipe "update_result" (shared_update ()) aaction
let p_parallel = let_pipe "flow_key" aparse (p_shared_update_1 ||| p_shared_update_2)


(*
let aparse = {
  name = "parse";
  arity = 2;
  output = "flow_key";
  f = fun arg_map -> let args = get_arguments ["pkt_len"; "packet"] arg_map in StringMap.add "flow_key" (parse (List.nth args 0) (List.nth args 1)) arg_map
}

let aupdate = {
  name = "update";
  inputs = ["flow_key"];
  output = "update_result";
  f = fun arg_map -> let args = get_arguments ["flow_key"] arg_map in StringMap.add "update_result" (update (List.nth args 0)) arg_map
}

let aaction = {
  name = "action";
  inputs = ["update_result"; "pkt_len"; "packet"];
  output = "flow_key";
  f = fun arg_map -> let args = get_arguments ["update_result"; "pkt_len"; "packet"] arg_map in StringMap.add "nothing" (action (List.nth args 0) (List.nth args 1) (List.nth args 2)) arg_map
}
*)
let init a =
  let argc = Array.length a in 
  let argv = CArray.of_list string (Array.to_list a) in
  let pipe = setup aparse in 
  run argc (CArray.start argv) (eval pipe);;

init Sys.argv;;
(*print_endline (Int.to_string (test_func 1 2))*)

(*
let make_handle_packet pipeline = 
  let handle_packet pkt_len packet = 
    let labels : (unit ptr) StringMap.t = StringMap.empty in
    let labels = StringMap.add "packet" packet labels in
    let labels = StringMap.add "pkt_len" pkt_len labels in


;;


let pipe1 = 
  start switch >>> aparse
;;

(* helper to test a pipeline *)
let test_pipe name pipe item_printer = 

  print_endline ("--- "^name^" ---");
  let start, end_, _ = infer_locations pipe in
  print_endline ("pipe spans from "^(Loc.to_strings start)^" to "^(Loc.to_strings end_));

  let file = "input.pcap" in
  let ha = get_handle file in
  let hdr = make pcap_pkthdr in
  let p = load_next_pcap_pkt ha hdr in

  let results = exec pipe in
  List.iter (fun (loc, output) -> 
    print_endline ((item_printer output)^" from "^(Loc.to_string loc))) results
;;






main ();;*)