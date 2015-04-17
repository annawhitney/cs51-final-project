open Core.Std

(* A module signature for an imperative priority heap. *)
module type PRIOHEAP =
sig

  (* The type of an element in the heap. *)
  type elt
  
  (* An abstract type for the heap. *)
  type heap

  (* Returns an empty heap. *)
  val empty: unit -> heap

  (* Inserts an element into the heap. *)
  val insert: elt -> heap -> unit

  (* Removes the minimum element from the heap and returns it.
   * If heap is empty, returns None. *)
  val delete_min: heap -> elt option

  (* Decreases the key of the specified element of the heap. *)
  val decrease_key: elt -> elt -> heap -> unit
