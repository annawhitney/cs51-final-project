open Core.Std
open Graph

exception TODO

(* Read in .csv file from user input and store in adjacency list *)
(* NOTE: We probably want this function to return the graph the csv
 * was read into! *)
(* We also need to look at pset 7 to figure out how to handle cmd line args *)
let read_csv : fun () -> () = TODO ;;

(* Request start and finish nodes from user *)
let get_nodes : fun () -> string * string = 
  (* Should give the user a text prompt so they know what to input *)
  let () = Printf.printf "Starting Point: " in
  let st = read_line () in
  (* we need to make sure st is contained in our imported file *) 
  let () = Printf.printf "End Point: " in
  (* same goes with fin *) 
  let fin = read_line () in
  st, fin ;;

(* Run dijkstra's algorithm to find shortest path between start and finish *)
let dijkstra (st: node) (fin: node) (g: GeoGraph.graph) : node list =
  (* Initialize empty heap *)
  let fib_heap = FibHeap.empty in
  (* Insert all nodes into heap with initial distance of infinity (using
   * Float.max_value to represent infinity) *)
  List.fold_left (GeoGraph.nodes g)
      ~f:(fun _ s -> FibHeap.insert Float.max_value s fib_heap) ~i:() ;
  (* TODO: continue algorithm *)

dijkstra st fin (read_csv (* cmd line arg *)) ;;
