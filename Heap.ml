open Core.Std
open Order

(* A module signature for an imperative priority heap. *)
module type PRIOHEAP =
sig

  (* The type of keys and values in the heap. *)
  type key
  type value
  
  (* An abstract type for the heap. *)
  type heap

  (* Returns an empty heap. *)
  val empty: unit -> heap

  (* Inserts an element into the heap. *)
  val insert: key -> value -> heap -> unit

  (* Removes the minimum-key element from the heap and returns it.
   * If heap is empty, returns None. *)
  val delete_min: heap -> (key * value) option

  (* Decreases the key of the specified element of the heap. *)
  val decrease_key: key -> key -> heap -> unit

end

module type HEAP_ARG =
sig

  type key
  type value

  val compare: key -> key -> Ordering.t
end

module FibonacciHeap(H: HEAP_ARG) : (PRIOHEAP with type key = H.key
    with type value = H.value) =
struct
  type key = H.key
  type value = H.value
  type heap = TODO (* Doubly linked list of heap-ordered trees *)

  let cut = TODO
  let merge = TODO
  let mark = TODO

  let insert = TODO
  let delete_min = TODO
  let decrease_key = TODO
end
