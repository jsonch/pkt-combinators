(* Locations in the pipeline *)

type t = 
| Network
| Switch 
| NIC
| Core of int
| Unbound of int 
;;

let switch = Switch 
let nic = NIC
let core x = Core(x)
let cores xs = List.map core xs

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
let to_string loc = 
  match loc with
  | Network -> "Network"
  | Switch -> "Switch"
  | NIC -> "NIC"
  | Core i -> "Core " ^ string_of_int i
  | Unbound i -> "Unbound " ^ string_of_int i
;;  
let to_strings locs = 
  "["^(List.map to_string locs |> String.concat ", ")^"]"
;;