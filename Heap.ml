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
  (* open Order (we don't have an Order file :/ ) *)

  type key
  type value

  val compare : key -> key -> Ordering.t

  (* For testing purposes *)
  val string_of_key : key -> string
  val string_of_value : value -> string

  (* Should return same key every time given same inputs*)
  val gen_key : unit -> key
  val gen_key_gt : key -> unit -> key
  val gen_key_lt : key -> unit -> key
  val gen_key_between : key -> key -> unit -> key option

  (* Should return random key *)
  val gen_key_random : unit -> key
  val gen_value : unit -> value
  val gen_pair : unit -> key * value
end

module FibonacciHeap(H: HEAP_ARG) : (PRIOHEAP with type value = H.value
    and type key = H.key) =
struct
  type key = H.key
  type value = H.value
  type pair = key * value
  type rank = int ref
  type marked = bool ref
  (* A heap will consist of either a Leaf ref (empty heap), or of 
   * ((k,v), parent ref, left sib ref, right sib ref,
   * child ref, no. of children, child cut) *)
  type heap = tree ref
  (* This tree data type is not a regular tree; the root of this tree 
   * can have sibling roots and the root can also have a parent. A tree 
   * is nothing more than a dereferenced heap in this code *)
  and tree = 
  | Leaf
  | Node of pair * heap * heap * heap * heap * rank * marked

  let empty : heap = ref Leaf

  let isempty (h: heap) : bool =
    match !h with
    | Leaf -> true
    | _ -> false

  let minkey (k1: key) (k2: key) : key =
    match H.compare k2 k1 with
    | Less -> k2
    | _ -> k1

  (* Returns heap with smaller priority root key; if keys equal, first arg 
   * returned. Empty heap considered smaller than all non-empty heaps *)
  let minroot (t1: tree) (t2: tree) : tree =
    match t2 with
    | Leaf -> t1
    | Node((k2,_),_,_,_,_,_,_) ->
      match t1 with
      | Leaf -> t2
      | Node((k1,_),_,_,_,_,_,_) ->
	if minkey k1 k2 = k2 then t2 else t1

  (* Returns smallest root node in heap *)
  let leastroot (h: heap) : tree = 
    let rec leastroot_helper (t: tree) (h0: heap) : tree =
      (match t with
      | Leaf -> failwith "node must have siblings"
      | Node(_,_,l,_,_,_,_) -> 
	if phys_equal l h0
	then t
	else minroot t (leastroot_helper !l h0)) in
    match !h with
    | Leaf -> !h
    | Node(_,_,_,r,_,_,_) ->
      match !r with
      | Leaf -> !h
      | Node(_,_,l,_,_,_,_) -> leastroot_helper !h l

(*
(* Old implementation of insert; delete when finished *)
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
*)

  (* treats a node as orphaned and w/out siblings and inserts into a heap *)
  let general_insert (t: tree) (h: heap) : heap =
    match !h with
    | Leaf -> ref t
    | Node((hk,hv),hp,hl,hr,hc,hrk,hm) ->
      match t with
      | Leaf -> h
      | Node((k,v),_,_,_,c,rk,m) ->
	match !hl with
	| Leaf -> 
	  let newnode = Node((k,v), empty, h, h, c, rk, m) in
	  h := Node((hk,hv), hp, ref newnode, ref newnode, hc, hrk, hm);
	  ref newnode
	| Node(lkv,lp,ll,lr,lc,lrk,lm) ->
	  let newnode = Node((k,v), empty, hl, h, c, rk, m) in
	  hl := Node(lkv, lp, ll, ref newnode, lc, lrk, lm);
	  h := Node((hk,hv), hp, ref newnode, hr, hc, hrk, hm);
	  ref newnode

  let insert (k: key) (v: value) (h: heap) : heap =
    let newheap = 
      general_insert (Node((k,v),empty,empty,empty,empty,ref 0,ref false)) h in
    ref (minroot !h !newheap)

  (* merges orphaned tree w/out siblings w/ anothe tree, preserves invariants *)
  let merge (singlet: tree) (t: tree) : tree =
    match singlet with
    | Leaf -> t
    | Node(skv,_,_,_,sc,srk,sm) ->
      match t with
      | Leaf -> singlet
      | Node(kv,p,l,r,c,rk,m) ->
	if minroot singlet t = singlet
	then
	  Node(skv, p, l, r, general_insert t sc, ref (!srk+1), sm)
	else
	  Node(kv, p, l, r, general_insert singlet c, ref (!rk+1), m)
    

  let decrease_key = fun _ _ _ -> ()
  let delete_min = fun _ -> None

  (* cut removes and returns a tree from the surrounding heap. 
   * cut doesn't change parent marked, but it does decrease parent rank *)
  let cut (t: tree) : tree =
    match t with
    | Leaf -> Leaf
    | Node(kv,p,l,r,c,rk,m) ->
      match !p with
      (* cut need never be performed on a root node *)
      | Leaf -> failwith "node must be a tree"
      | Node(_,_,_,_,pc,_,_) ->
	pc := !l; rk := !rk+1;
	match !l with
	| Leaf -> Node(kv,empty,empty,empty,c,rk,m)
	| Node(_,_,_,lr,_,_,_) ->
	  lr := !r;
	  match !r with
	  | Leaf -> failwith "node must be a tree"
	  | Node(_,_,rl,_,_,_,_) ->
	    rl := !l;
	    Node(kv,empty,empty,empty,c,rk,m)
	  

(*
  let merge = TODO
  let mark = TODO

*)
end
(*
(* for use in testing *)
module ListArg(C : COMPARABLE) : (HEAP_ARG with type value = int list
			   and type key = C.t) =
struct
  type key = C.t
  type value = int list
  let compare (k1: key) (k2:key) : Ordering.t =
    if k1 > k2
    then Greater
    else 
      if k1 = k2
      then Equal
      else Less
end
*)
