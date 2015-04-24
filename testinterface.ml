open Core.Std

(* just making sure that get_nodes works *)

let get_nodes : string * string  = 
  let () = Printf.printf "Starting Point: " in
  let st = read_line() in
  (* make sure st is actually a node *)
  let () = Printf.printf "End Point: " in
  let fin = read_line() in 
  st, fin;;


