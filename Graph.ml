open Core.Std
open Dict

module type NODE =
sig
  type node
  type weight
  (* Allows us to search for a node *)
  type tag

  (* Require that nodes be comparable for efficiency. *)
  val compare : node -> node -> Ordering.t
  val string_of_node : node -> string
  val string_of_weight : weight -> string
  val gen : unit -> node
  val gen_weight : unit -> weight
  val tag_of_node : node -> tag
  val node_of_tag : tag -> node
end

(* A signature for directed graphs with unweighted edges *)
(* TODO: Modify this to be an undirected graph and use weighted edges! *)
(* This definitely involves modifying NeighborSet to be NeighborDict instead
 * (see below) but we might need to define other functions in the interface
 * so we can get out edge weights and do operations on them. *)
module type GRAPH =
sig
  module N : NODE
  type node = N.node
  type weight = N.weight
  type tag = N.tag
  type graph

  val empty : graph

  val nodes : graph -> node list

  val is_empty : graph -> bool

  val add_node : graph -> node -> graph

  (* Adds the nodes if they aren't already present. *)
  val add_edge : graph -> node -> node -> weight -> graph

  (* Return None if node isn't in the graph *)
  val neighbors : graph -> node -> (node * weight) list option

  (* Return None if node isn't in the graph *)
  val outgoing_edges : graph -> node -> (node * node * weight) list option

  val has_node : graph -> node -> bool

  (* Finds a node by its tag; returns None if node is not in graph. *)
  val get_node_by_tag : graph -> tag -> node option

  (* Create a node from a given tag. *)
  val node_of_tag : tag -> node

  (* Return None if the graph is empty *)
  val get_random_node : graph -> node option

  val string_of_graph : graph -> string
end

module Graph(NA: NODE) : (GRAPH with module N = NA) =
struct
  open Order;;
  module N = NA
  type node = N.node
  type weight = N.weight
  type tag = N.tag

  (* We'll represent a graph as an edge dictionary:
     dictionary: node -> neighbor dict (TODO!)
     Every node in the graph must be a key in the dictionary.
  *)

  (* TODO: replace this with an analogous NeighborDict using Dict.Make
   * with keys that are nodes and values that are edge weights *)
  
  module NeighborDict = Dict.Make(
     struct
        type key = node
        type value = weight
        let compare = N.compare
        let string_of_t = N.string_of_node
        let gen_key = N.gen
        let gen_key_random = N.gen
        let gen_key_gt _ () = N.gen ()
        let gen_key_lt _ () = N.gen ()
        let gen_key_between _ _ () = None
        let gen_value = N.gen_weight
        let gen_pair () = (gen_key (), gen_value ())
        let string_of_value = N.string_of_weight
        let string_of_key = N.string_of_node
      end)

  (* TODO: Change all mentions of NeighborSet in here to NeighborDict, but
   * otherwise this should stay pretty much the same *)
  module EdgeDict = Dict.Make(
    struct
      type key = node

      (* neighbordict.dict or neighbordict.set ?? *)
      type value = NeighborDict.dict
      let compare = N.compare
      let string_of_key = N.string_of_node
      let string_of_value ns = NeighborDict.string_of_dict ns
      let gen_key = N.gen
      let gen_key_random = N.gen
      let gen_key_gt _ () = N.gen ()
      let gen_key_lt _ () = N.gen ()
      let gen_key_between _ _ () = None
      let gen_value () = NeighborDict.empty
      let gen_pair () = (gen_key(),gen_value())
    end)

  (* Unclear if we need this or not - as far as I can tell, they only use it
   * to pick a random node, which we don't need to do, but let's leave it in
   * for now; might be useful for something *)
  module IntNode = Dict.Make(
    struct
      type key = int
      type value = node
      let compare = int_compare
      let string_of_key = string_of_int
      let string_of_value = N.string_of_node
      let gen_key () = 0
      let gen_key_random () = 0
      let gen_key_gt _ () = 1
      let gen_key_lt _ () = (-1)
      let gen_key_between _ _ () = None
      let gen_value = N.gen
      let gen_pair () = (gen_key(),gen_value())
    end)

 type graph = { edges : EdgeDict.dict ;
                num_nodes : int ;
                index_to_node_map : IntNode.dict }

 let empty : graph = { edges = EdgeDict.empty;
                       num_nodes = 0;
                       index_to_node_map = IntNode.empty }

 let add_node g n =
   if EdgeDict.member g.edges n then g
   else
     { edges = EdgeDict.insert g.edges n NeighborDict.empty ;
       num_nodes = g.num_nodes + 1 ;
       index_to_node_map =
         IntNode.insert g.index_to_node_map g.num_nodes n }

  let nodes g =
    EdgeDict.fold (fun k _ r -> k :: r) [] g.edges

  let is_empty g = (g.num_nodes = 0)

  (* TODO: Modify to take edge weight into account and to be undirected
   * (i.e., we have to add the edge to the list of BOTH nodes, not just one) *)
  (* Adds the nodes if they aren't already present. *)
  (* val would be the weight *)
  let add_edge g orig dest v =
    let half_add g src dst v = 
      let new_neighbors = (match EdgeDict.lookup g.edges src with
        | None -> NeighborDict.insert NeighborDict.empty dst v
        | Some s -> NeighborDict.insert s dst v)
      in
      (* ensure both src and dst in the graph before adding edge *)
      let g' = (add_node (add_node g src) dst) in
      {edges = EdgeDict.insert g'.edges src new_neighbors;
       num_nodes = g'.num_nodes;
       index_to_node_map = g'.index_to_node_map}
    in
    ignore (half_add g orig dest v); half_add g dest orig v

  (* TODO: Modify to take edge weight into account *)
  let neighbors g n : (node * weight) list option =
    match EdgeDict.lookup g.edges n with
      | None -> None
      | Some s -> Some (NeighborDict.fold (fun neigh v r -> (neigh, v) :: r) [] s)

  (* TODO: Modify to take edge weight into account *)
  let outgoing_edges g src : (node * node * weight) list option =
    match EdgeDict.lookup g.edges src with
      | None -> None
      | Some s -> Some (NeighborDict.fold (fun dst v r ->
                                             (src, dst, v) :: r) [] s)

  let has_node g n =
    match EdgeDict.lookup g.edges n with
      | None -> false
      | _ -> true

  (* Added this function for convenience in Dijkstra algorithm *)
  let get_node_by_tag g t : node option =
    EdgeDict.verify_key g.edges (N.node_of_tag t)

  let node_of_tag = N.node_of_tag

  let get_random_node g =
    if g.num_nodes = 0 then None else
      let r = Random.int (g.num_nodes) in
        IntNode.lookup g.index_to_node_map r

  let string_of_graph g =
    "Graph: " ^ (EdgeDict.string_of_dict g.edges)
end
(*
module NamedGraph =
struct
  include(Graph(struct
                  type node = string
                  type weight = int
                  let compare = Order.string_compare
                  let string_of_node = fun x -> x
                  let gen () = ""
                end))
  let from_edges (es: (string * string) list) : graph =
    List.fold_left es ~f:(fun g (src, dst) -> add_edge g src dst) ~init:empty
end

(* TODO: update tests to incorporate weights so we can make sure our weighted
 * graph implementation works. *)
(* Wrap our tests in a module so that they don't pollute the namespace *)
module TestGraph =
struct
  module G = NamedGraph

  let g = G.add_edge G.empty "a" "b" 4.;;
  let g2 = G.add_edge g "a" "c" 2.;;

  let deopt_len lo =
    match lo with
      | None -> 0
      | Some xs -> List.length xs;;

  let deopt_lst lo =
    match lo with
      | None -> []
      | Some xs -> xs;;

  let deopt_node no =
    match no with
      | None -> "None"
      | Some n -> n;;

  let _ = (
    assert (not(G.has_node g "a"));
    assert (G.has_node g "a");
    assert (G.has_node g "b");
    assert (G.has_node g "c" = false);
    assert (G.has_node g2 "c");
    assert (G.has_node g2 "d" = false);

    assert (List.length (G.nodes G.empty) = 0) ;
    assert (List.length (G.nodes (G.add_node G.empty "a")) = 1) ;

    assert (List.length (G.nodes g) = 2) ;

    assert (List.length (G.nodes g2) = 3) ;

    assert (deopt_len (G.outgoing_edges g2 "a") = 2) ;
    assert (deopt_len (G.outgoing_edges g2 "b") = 0) ;
    assert (deopt_len (G.outgoing_edges g2 "c") = 0) ;
    assert (G.outgoing_edges g2 "d" = None) ;

    assert (deopt_len (G.neighbors g2 "a") = 2) ;
    assert (deopt_len (G.neighbors g2 "b") = 0) ;
    assert (deopt_len (G.neighbors g2 "c") = 0) ;
    assert (G.neighbors g2 "d" = None) ;

    assert (let t = deopt_lst (G.neighbors g2 "a") in
              t = [("b", 4.);("c", 2.)] || t = [("c", 2.);("b", 4.)]) ) 
end
*)



