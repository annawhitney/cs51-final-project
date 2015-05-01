open Core.Std
open Graph
open Heap
open Distance

let rec addedges (castlist: string * (float * float) list) 
(graph: Geograph.graph) (cutoff: float) : Geograph.graph = 
  let rec addhelper (place: string * (float * float)) (rest: string * (float * float) list)
    (graph: Geograph.graph) : Geograph.graph = 
    match rest with
    | [] -> graph
    | (name2, loc2)::tl -> (let (name1, loc1) = place in
                           match distance cutoff loc1 loc2 with
                           | None -> addhelper place tl graph
                           | Some d -> Geograph.add_edge graph
                                       (GeoNode.node_of_tag name1)
                                       (GeoNode.node_of_tag name2) d; 
                                       addhelper place tl graph) in
  match castlist with
  | [] -> graph
  | hd::tl -> ignore(addhelper hd tl graph); addedges tl graph cutoff

   

let read_csv () : GeoGraph.graph =  
  let usage () = Printf.print "usage: %s csv cutoff " Sys.argv.(0); exit 1 in 
  if Array.length Sys.argv <> 3 then usage () ;
  (* file pointer - just like in CS50! *)
  let cutoff = Sys.argv.(2) in
  let file = create Sys.argv.(1) in
  let lines = input_lines file in
  let delimiter = Str.regex ";" in
  let parse_line line = Str.split_delim delimiter line in
  let parsed : string list list = List.map lines ~f:parse_line in
  let rec cast (parselist: string list list) : string * (float * float) list =
     match parselist with
     | [] -> []  
     | hd::tl -> (match hd with 
       | name::lat::long -> name * 
         ((Float.of_string lat) * (Float.of_string long))::(casted tl)
       | _ -> []) in
  let casted = cast parsed in
  addedges casted Geograph.empty cutoff
  
  


  (* List.map/List.fold over parsed, plugging Float.of_string of the lat & lng 
   * parts (which you can access using List.nth, maybe?) into distance fn;
   * use GeoGraph.node_of_tag <string> to create a node from a city name, and
   * use GeoGraph.add_edge to add an edge between two nodes to the graph (ask
   * Alena for details about add_edge) *)

(*
  let newgraph = GeoGraph.empty   

*)

(*  
    distance


*)


   
 (*Sys.argv.(1) =  
  let cutoff = Sys.argv.(2); *)
