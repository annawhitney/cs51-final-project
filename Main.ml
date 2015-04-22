open Core.Std
open Graph

exception TODO

(* Read in .csv file from user input and store in adjacency list *)
(* NOTE: We probably want this function to return the graph the csv
 * was read into! *)
let read_csv : fun () -> () = TODO ;;

(* Request start and finish nodes from user *)
let get_nodes : fun () -> string * string = 
  (* Should give the user a text prompt so they know what to input *)
  let st = read_string () in
  let fin = read_string () in
  st, fin ;;

(* Run dijkstra's algorithm to find shortest path between start and finish *)
let dijkstra (st: node) (fin: node) (g: graph) : node list =
  (* match strings with nodes in adjacency list  and then run algorithm 
   * using fibonocci heap for storage and access *)
  TODO

read_csv () ;;
dijkstra () ;;
