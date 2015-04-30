open Core.Std
open Graph
open Heap
open Distance

let earthrad = 6371.0 

let pi = 4. *. atan 1.

(* haversin = (sin(theta/2))^2 *)
let haversin (x:float) : float =
  (sin (x /. 2.0)) ** 2.0

(* converts lat and long pairs from degrees into radians *)
let coords_to_rad (x: float * float) : float * float = 
  let radians =  ( *.) (pi /. 180.0) in
  match x with 
  | (a, b) -> (radians a, radians b)

(* our spherical distance function is all based on the Haversine Formula -
 * see https://www.youtube.com/watch?v=y1JZNTRDn_w *
 * also see http://www.movable-type.co.uk/scripts/latlong.html *)

let distance (cutoff: float) (st: float * float)
 (fin: float * float) : float option = 
  let (lat1, long1) = coords_to_rad st in 
  let (lat2, long2) = coords_to_rad fin in
  let a = haversin (lat2 -. lat1) +. ((cos lat1) *. (cos lat2) *. 
    haversin (long2 -. long1)) in
  let dis = 2.0 *. earthrad *. (atan2 (sqrt a) (sqrt (1.0 -. a))) in  
  if dis < cutoff then Some dis else None 

let rec addedges (castlist: string * (float * float) list) 
(graph: Geograph.graph) (cutoff: float) : Geograph.graph = 
  let rec addhelper (place: string * (float * float)) (rest: string * (float * float) list)
    (graph: Geograph.graph) : Geograph.graph = 
    match rest with
    | [] -> graph
    | (name2, loc2)::tl -> (let (name1, loc1) = place in
                           match distance cutoff loc1 loc2 with
                           | None -> addhelper place tl graph
                           | Some d -> Geograph.add_edge graph (GeoNode.node_of_tag name1) (GeoNode.node_of_tag name2) d; 
                                       addhelper place tl graph) in
  match castlist with
  | [] -> graph
  | hd::tl -> addhelper hd tl graph; addedges tl graph cutoff

   

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
