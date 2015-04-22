(****************************************
 * Fibonacci Heap module
 * CS51
 ***************************************)
open Core.Std

(* A module signature for an imperative priority heap. *)
module type PRIOHEAP =
sig

  (* The type of keys and values in the heap. *)
  type key
  type value
  
  (* An abstract type for the heap. *)
  type heap

  (* Returns an empty heap. *)
  val empty: heap

  (* Inserts an element into the heap. *)
  val insert: key -> value -> heap -> heap

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
  type pair = key * value
  type rank = int
  type marked = bool
  (* A heap will consist of either a Leaf ref (empty heap), or of 
   * ((k,v), parent ref, left sib ref, right sib ref,
   * child ref, no. of children, child cut) *)
  type heap = tree ref
  and tree = 
  | Leaf
  | Node of pair * heap * heap * heap * heap * rank * marked

  let empty : heap = ref Leaf

  let isempty (h: heap) : bool =
    match !h with
    | Leaf -> true
    | _ -> false

  (* Returns heap with smaller priority root key; if keys equal, first arg 
   * returned. Empty heap considered smaller than all non-empty heaps *)
  let minroot (h1: heap) (h2: heap) : heap =
    match !h2 with
    | Leaf -> h1
    | Node((k2,_),_,_,_,_,_,_) ->
      match !h1 with
      | Leaf -> h2
      | Node((k1,_),_,_,_,_,_,_) ->
	match H.compare k1 k2 with
	| Greater -> h2
	| _ -> h1

  let minkey (k1: key) (k2: key) : key =
    match H.compare k2 k1 with
    | Less -> k2
    | _ -> k1

  (* Returns ref to smallest root node in heap *)
  let leastroot (h: heap) : heap = 
    let rec leastroot_helper (h': heap) (h0: heap) : heap =
      (match !h' with
      | Leaf -> failwith "node must have siblings"
      | Node(_,_,l,_,_,_,_) -> 
	if phys_equal h' h0
	then h'
	else minroot h' (leastroot_helper l h0)) in
    match !h with
    | Leaf -> h
    | Node(_,_,_,r,_,_,_) ->
      match !r with
      | Leaf -> h
      | Node(_,_,l,_,_,_,_) -> leastroot_helper h l

(* TODO fix insert to point to correct node at end; i.e. check for min *)
  let insert (k: key) (v: value) (h: heap) : heap =
    match !h with
    | Leaf -> h := Node((k,v), empty, empty, empty, empty, 0, false); h
    | Node((hk,hv),hp,hl,hr,hc,hrk,hm) ->
      match !hl with
      | Leaf -> 
	let newnode = Node((k,v), empty, h, h, empty, 0, false) in
	h := Node((hk,hv), hp, ref newnode, ref newnode, hc, hrk, hm);
	if minkey hk k = hk then h else ref newnode
      | Node(lkv,lp,ll,lr,lc,lrk,lm) ->
	let newnode = Node((k,v), empty, hl, h, empty, 0, false) in
	hl := Node(lkv, lp, ll, ref newnode, lc, lrk, lm);
	h := Node((hk,hv), hp, ref newnode, hr, hc, hrk, hm);
	if minkey hk k = hk then h else ref newnode

  let merge (h1: heap) (h2: heap) : heap =
    if minroot h1 h2 = h1 
    then
      match !h1 with
      | Leaf -> (* TODO *) empty
      | Node(kv1,p1,l1,r1,c1,rk1,m1) -> (* TODO *) empty
    else
      match !h2 with
      | Leaf -> (* TODO *) empty
      | Node(kv2,p2,l1,r1,c2,rk2,m2) -> (* TODO *) empty
    
    

  let decrease_key = fun _ _ _ -> ()
  let delete_min = fun _ -> None

(*
  let cut = TODO
  let merge = TODO
  let mark = TODO

*)
end
(*
module ListArg
*)
