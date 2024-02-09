open CTypes
open 
type state = unit ptr
let state : state typ = ptr void

type atom_ret = 
| STOP
| CONTINUE

let atom_ret : atom_ret typ = 
  view ~read:(function
      | 0 -> STOP
      | 1 -> CONTINUE
      | _ -> failwith "Invalid atom_ret value"
    )
    ~write:(function
      | STOP -> 0
      | CONTINUE -> 1
    )
    int

(*the metadata type that gets accessed by all the atoms*)
type metadata
let metadata : metadata structure = structure "metadata"
(*Here the user has to tell us the fields in the metadata (aside from the default ones I guess)*)
let pkt_len = field metadata "pkt_len" uint32_t
let src_ip = field metadata "src_ip" uint32_t
let dst_ip = field metadata "dst_ip" uint32_t
let src_port = field metadata "src_port" uint16_t
let dst_port = field metadata "dst_port" uint16_t
let cur_state = field metadata "cur_state" uint32_t
let () = seal metadata

let parse = foreign ~from:libadd "parse" (state @->  (ptr void) @-> (ptr metadata) @-> returning atom_ret)


