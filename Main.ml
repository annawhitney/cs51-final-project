open Core.Std
open Heap
open Read

exception TODO

(* Read in .csv file from user input and store in adjacency list *)
(* NOTE: We probably want this function to return the graph the csv
 * was read into! *)
(* We also need to look at pset 7 to figure out how to handle cmd line args *)
(* read_csv is now done in the Read.ml file *)
(*let read_csv () : GeoGraph.graph = TODO ;;*)

(* Request start and finish nodes from user *)
let get_nodes (g: GeoGraph.graph) : GeoNode.node * GeoNode.node =
  (* get_nodes should actually return an option GeoNode.node * GeoNode.node *) 
  (* Should give the user a text prompt so they know what to input *)
  let () = Printf.printf "Starting Point: " in
  let st = read_line () in
  let stnode = node_of_tag st in
  if !(hasnode g stnode) then None else 
    let () = Printf.printf "End Point: " in
    let fin = read_line () in
    let finnode = node_of_tag fin in
    if !(hasnode g finnode) then None else
  (* NOTE: Don't just return the strings directly - look up the strings in the
   * graph and return their node counterparts. If they're not in the graph,
   * re-prompt the user. *)
    Some (stnode, finnode) ;;

(* Run dijkstra's algorithm to find shortest path between start and finish *)
let dijkstra (st: GeoNode.node) (fin: GeoNode.node) (g: GeoGraph.graph)
    : (GeoNode.node list) * GeoNode.weight =
  
  (* Initialize heap containing only source node with distance of 0 *)
  let with_source = FibHeap.insert 0 st FibHeap.empty in
  (* Insert all other nodes into heap with initial distance of infinity (using
   * Float.max_value to represent infinity) and hold on to a pointer to each *)
  let insert_not_source (h: FibHeap.heap) (s: GeoNode.node) =
    match (GeoNode.compare s st) with
    | Less | Greater -> 
        let (hp,nd) = FibHeap.insert Float.max_value s h in
        s.pt <- Some nd ; hp
    | Equal -> h
  in
  let fib_heap = List.fold_left (GeoGraph.nodes g) ~f:insert_not_source
      ~i:with_source in

  (* Keep taking min, checking its neighbors, and updating distances until
   * our destination node is the min that we take. *)
  let rec next_node (h: FibHeap.heap) (prev: string Links.link_node) =
    let (min,hp) = FibHeap.delete_min h in
    match min with
    | None -> failwith "heap empty without reaching destination"
    | Some (dist,nm) ->
        (* Find corresponding node in graph *)
        (match GeoGraph.get_node_by_tag nm with
        | None -> failwith "heap min is not in graph"
        | Some this_nd ->
            (* Record that we've visited this node *)
            let _ = this_nd.pt <- None in
            (* Record where we've been *)
            let _ = this_nd.prev <- Link prev in
            (* If the min node we pulled was our destination, we're done;
             * return distance and list of nodes in the shortest path *)
            (match GeoNode.compare this_nd fin with
            | Equal ->
                (Links.list_of_links (Node (this_nd.name,this_nd.prev)),dist)
            | Less | Greater ->
                (* Otherwise, get the neighbors of our min *)
                (match GeoGraph.neighbors g this_nd with
                | None -> failwith "we already checked that this_node exists"
                | Some ns ->
                    (* For each neighbor, update distance if necessary *)
                    let handle_node (h: FibHeap.heap) (n,w) =
                      let alt = dist + w in
                      (* If no heap pointer associated with this node, we must
                       * have visited it already, so don't decrease any key *)
                      (match n.pt with
                      | None -> h
                      | Some pnt ->
                          (* Otherwise, decrease key of appropriate node *)
                          (match FibHeap.get_top_node pnt with
                          | None -> failwith "empty heap node"
                          | Some (k,v) -> assert(v = n.name) ;
                              if alt < k then FibHeap.decrease_key pnt alt h
                              else h))
                    in
                    next_node (List.fold_left ns ~f:handle_node ~i:hp)
                        (Node (nm,Link(ref prev))))))
  in next_node fib_heap ;;

let graph = read_csv (* cmd line arg *) in
let (start,finish) = get_nodes graph in
dijkstra start finish graph ;;
(* TODO: which parts do we actually want to print out? *)
