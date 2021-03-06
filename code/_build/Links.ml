open Core.Std

(* A singly linked list implementation that, unlike OCaml's built-in version,
 * allows for multiple nodes to point to the same next node. *)
type 'a link = Nil | Link of 'a link_node ref
and 'a link_node = Node of ('a * 'a link)

(* Turns a link_node-based list into a normal OCaml list. *)
let list_of_links (ll: 'a link_node) : 'a list =
  let rec lol_helper ll lst =
    let Node (a,lk) = ll in
    match lk with
    | Nil -> a::lst
    | Link r -> lol_helper !r (a::lst)
  in
  lol_helper ll []
