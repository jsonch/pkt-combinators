[@@@ocaml.warning "-33"]
[@@@ocaml.warning "-32"]
[@@@ocaml.warning "-34"]
open Ctypes
open Foreign

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
let parse = foreign ~from:libknock "parse" (pkt_len @-> packet @-> returning flow_key)
let update =  foreign ~from:libknock "update" (flow_key @-> returning update_result)
let action = foreign ~from:libknock "action" (update_result @-> pkt_len @-> packet @-> returning update_result)

(*let get_handle = foreign ~from:libknock "get_pcap_handle" (Ctypes.string @-> returning pcap_handle)
let load_next_pcap_pkt = foreign ~from:libknock "load_next_pcap_pkt" (pcap_handle @-> pcap_pkthdr @-> returning (ptr char))
*)
module StringMap = Map.Make(String);;
type atom = 
{ name : string;
  inputs : string list;
  output : string;
  f : (unit ptr) StringMap.t -> (unit ptr) StringMap.t;
}

type pipe =
  Atom: atom -> pipe
  | Seq : pipe * pipe -> pipe

(*OCaml wrappers for how to call the above functions using the labels
   I can't figure out how to do this generally...*)
let get_arguments arguments arg_map : 'a list = List.map (fun arg -> match StringMap.find_opt arg arg_map with
      | Some(value) -> value
      | None -> failwith ("Missing argument: " ^ arg)) arguments


let aparse = {
  name = "parse";
  inputs = ["pkt_len"; "packet"];
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

let p1 = Seq ((Seq (Atom aparse, Atom aupdate)), Atom aaction)

let rec eval pipeline = 
    match pipeline with 
    Atom (a) -> a.f
    | Seq (p1, p2) -> fun m -> eval p2 (eval p1 m) 


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