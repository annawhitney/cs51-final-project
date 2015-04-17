open Core.Std

exception TODO

module type DICT =
sig
  type node
  type neighbor
  type distance
  type dict

(* An empty dictionary *)
  val empty : dict

  val fold : (node -> neighbour -> 'a -> 'a) -> 'a -> dict -> 'a
  
  val lookup : dict -> node -> neighbour option
  val member : dict -> node -> bool

  val insert : dict -> node -> neighbour -> dict
  val remove : dict -> node -> dict
  
  val choose : dict -> (node * neighbour * dict) option

  val string_of_node: node -> string
  val string_of_neighbour : neighbour -> string
  val string_of_dict : dict -> string

    val run_tests: unit -> unit

end

module type TwoThreeTree
sig
  type key
  type value
  type pair 
  type dict

  type kicked 
  type hole 
  type direction2
  type direction3

  val fold: (key -> value -> 'a -> 'a) -> 'a -> dict -> 'a 

  val insert_upward_two: pair -> dict -> dict -> pair -> dict -> kicked
  val insert_upward_three: pair -> dict -> dict -> pair -> pair -> dict -> dict -> kicked

  val insert: dict -> key -> value -> dict

  val insert_downward : dict -> key -> value -> kicked
  val insert_downward_three : pair -> pair -> pair -> dict -> dict -> dict -> kicked
  val insert_downward_two: pair -> pair -> dict -> dict -> kicked

  val remove_upward_two: pair -> pair option -> dict -> right ->  direction2 -> hole

  val remove_upward_three: pair -> pair -> pair option ->  dict ->  dict ->  dict ->  dict ->  direction3 -> hole

  val remove_downward: dict -> key -> hole

  val remove_downward_two: key -> pair -> dict -> dict -> hole

  val remove_downward_three: key ->  pair ->  pair ->  dict ->  dict ->  dict ->  hole

  val remove_min: dict -> hole

  val remove: dict -> key -> dict

  val lookup: dict -> key -> value option

  val choose: dict -> (key * value * dict) option

  val balanced: dict -> bool
  
  val get_edge_weight: dict -> value


end
