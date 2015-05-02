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

(* This has been modified from Moogle to now be an
   undirected graph with weighted edges *)
module type GRAPH =
sig
  type node
  type weight
  type tag
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

  (* Return None if the graph is empty *)
  val get_random_node : graph -> node option

  val string_of_graph : graph -> string
end

module Graph(N: NODE) : (GRAPH with type node = N.node
    with type weight = N.weight with type tag = N.tag) =
struct
  open Order;;
  type node = N.node
  type weight = N.weight
  type tag = N.tag

  (* We'll represent a graph as an edge dictionary:
     dictionary: node -> neighbor dict (TODO!)
     Every node in the graph must be a key in the dictionary.
  *)


  (* This NeighborDict will have keys which are nodes and
     values which are the weights/distances to that node
   *)  
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

  (* This EdgeDict connects nodes to NeighborDicts.
     Each node will have associated with it all its neighbors
     and the distances to them.
  *)
  module EdgeDict = Dict.Make(
    struct
      type key = node
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

  (* This IntNode is mainly used for testing *)
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
 
 (* checks if a node is in the graph and adds it if it's not in the graph *)
 let add_node g n =
   if EdgeDict.member g.edges n then g
   else
     { edges = EdgeDict.insert g.edges n NeighborDict.empty ;
       num_nodes = g.num_nodes + 1 ;
       index_to_node_map =
         IntNode.insert g.index_to_node_map g.num_nodes n }

  (* converts the graph into a list of nodes *)
  let nodes g =
    EdgeDict.fold (fun k _ r -> k :: r) [] g.edges

  let is_empty g = (g.num_nodes = 0)

  (* using a helper function which adds an edge between two nodes,
     this will add an edge of weight v from origin to destination
     and vice versa. This makes the graph undirected.
   *)
  let add_edge g orig dest v =
    let half_add g src dst v = 
      let new_neighbors =
        (match EdgeDict.lookup g.edges src with
        | None -> NeighborDict.insert NeighborDict.empty dst v
        | Some s -> NeighborDict.insert s dst v)
      in
      (* ensure both src and dst in the graph before adding edge *)
      let g' = (add_node (add_node g src) dst) in
      {edges = EdgeDict.insert g'.edges src new_neighbors;
       num_nodes = g'.num_nodes;
       index_to_node_map = g'.index_to_node_map}
    in
    half_add (half_add g dest orig v) orig dest v

  (* This converts a node's NeighborDict into a list of nodes and their weights *)
  let neighbors g n : (node * weight) list option =
    match EdgeDict.lookup g.edges n with
      | None -> None
      | Some s -> Some (NeighborDict.fold (fun neigh v r -> (neigh, v) :: r) [] s)

  (* This does essentially the same as above but was part of the Moogle structure *)
  let outgoing_edges g src : (node * node * weight) list option =
    match EdgeDict.lookup g.edges src with
      | None -> None
      | Some s -> Some (NeighborDict.fold (fun dst v r ->
                                             (src, dst, v) :: r) [] s)

  let has_node g n =
    match EdgeDict.lookup g.edges n with
      | None -> false
      | _ -> true

  (* Added this function for convenience in Dijkstra algorithm 
     It allows us to look up a node using its "tag"--essentially its
     name in string form
   *)
  let get_node_by_tag g t : node option =
    EdgeDict.verify_key g.edges (N.node_of_tag t)

  let get_random_node g =
    if g.num_nodes = 0 then None else
      let r = Random.int (g.num_nodes) in
        IntNode.lookup g.index_to_node_map r

  let string_of_graph g =
    "Graph: " ^ (EdgeDict.string_of_dict g.edges)
end

(* this module is for testing purposes *)
module NamedGraph =
struct
  include(Graph(struct
                  type node = string
                  type weight = int
                  type tag =  string 
                  let compare = Order.string_compare
                  let string_of_node = fun x -> x
                  let gen () = ""
                  let node_of_tag t = t
                  let tag_of_node n = n
                  let gen_weight () = 1 
                  let string_of_weight w = Int.to_string w 
                end))
  let from_edges (es: (string * string) list) : graph =
    List.fold_left es ~f:(fun g (src, dst) -> add_edge g src dst 1) ~init:empty
end



(* Wrap our tests in a module so that they don't pollute the namespace *)
module TestGraph =
struct
  module G = NamedGraph

  let g = G.add_edge G.empty "a" "b" 4;;
  let g2 = G.add_edge g "a" "c" 2;;

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
    assert (deopt_len (G.outgoing_edges g2 "b") = 1) ;
    assert (deopt_len (G.outgoing_edges g2 "c") = 1) ;
    assert (G.outgoing_edges g2 "d" = None) ;

    assert (deopt_len (G.neighbors g2 "a") = 2) ;
    assert (deopt_len (G.neighbors g2 "b") = 1) ;
    assert (deopt_len (G.neighbors g2 "c") = 1) ;
    assert (G.neighbors g2 "d" = None) ;

    assert (let t = deopt_lst (G.neighbors g2 "a") in
              t = [("b", 4);("c", 2)] || t = [("c", 2);("b", 4)]) ) 
end




