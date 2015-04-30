open Core.Std
open Graph
open Heap

let read_csv () : GeoGraph.graph = TODO 
  let usage () = Printf.print "usage: %s csv cutoff " Sys.argv.(0); exit 1 in 
  if Array.length Sys.argv <> 3 then usage () ;
  (* file pointer - just like in CS50! *)
  let file = create Sys.argv.(1) in
  let lines = input_lines file in
  let delimiter = Str.regex ";" in
  let parse_line line = Str.split_delim delimiter line in
  let parsed : string list list = List.map lines ~f:parse_line in
  (* List.map/List.fold over parsed, plugging Float.of_string of the lat & lng 
   * parts (which you can access using List.nth, maybe?) into distance fn;
   * use GeoGraph.node_of_tag <string> to create a node from a city name, and
   * use GeoGraph.add_edge to add an edge between two nodes to the graph (ask
   * Alena for details about add_edge) *)
  let newgraph = GeoGraph.empty   


   
 (*Sys.argv.(1) =  
  let cutoff = Sys.argv.(2); *)
