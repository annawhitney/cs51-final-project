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

  (* Runs all the tests. *)
  val run_tests: unit -> unit

end

module type HEAP_ARG =
sig
  type key
  type value

  val compare : key -> key -> Ordering.t

  (* For testing purposes *)
  val string_of_key : key -> string
  val string_of_value : value -> string

  (* Should return same key every time given same inputs *)
  val gen_key : unit -> key
  val gen_key_gt : key -> unit -> key
  val gen_key_lt : key -> unit -> key
  val gen_key_between : key -> key -> unit -> key option

  (* Should return random key *)
  val gen_key_random : unit -> key
  val gen_value : unit -> value
  val gen_pair : unit -> key * value
end

(* Borrowed wholesale from Moogle (since a HEAP_ARG and a DICT_ARG are
 * identical) for testing purposes *)
module IntStringHeapArg : HEAP_ARG =
struct
  open Order
  type key = int
  type value = string
  let compare x y = if x < y then Less else if x > y then Greater else Equal
  let string_of_key = string_of_int
  let string_of_value v = v
  let gen_key () = 0
  let gen_key_gt x () = x + 1
  let gen_key_lt x () = x - 1
  let gen_key_between x y () =
    let (lower, higher) = (min x y, max x y) in
    if higher - lower < 2 then None else Some (higher - 1)
  let gen_key_random =
    let _ = Random.self_init () in
    (fun () -> Random.int 10000)

  (* returns the nth string in lst, or "cow" n > length of list *)
  let rec lst_n (lst: string list) (n: int) : string =
    match lst with
      | [] -> "cow"
      | hd::tl -> if n = 0 then hd else lst_n tl (n-1)

  (* list of possible values to generate *)
  let possible_values = ["a";"c";"d";"e";"f";"g";"h";"i";"j";"k";"m";"n";
                         "o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z";
                         "zzzzzz";"cheese";"foo";"bar";"baz";"quux";"42"]
  let num_values = List.length possible_values
  (* gen_value will return the string at this current index *)
  let current_index = ref 0
  let gen_value () =
    let index = !current_index in
    if index >= num_values then
      (current_index := 0; lst_n possible_values index)
    else
      (current_index := index + 1; lst_n possible_values index)
  let gen_pair () = (gen_key_random(), gen_value())
end

module FibonacciHeap(H: HEAP_ARG) : (PRIOHEAP with type value = H.value
    and type key = H.key) =
struct
  type key = H.key
  type value = H.value
  type pair = key * value
  type rank = int
  type marked = bool
  (* A heap will consist of either a Leaf ref (empty heap), or of 
   * ((k,v), parent ref, left sib ref, right sib ref,
   * child ref, no. of children (rank), child cut (marked)) *)
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
      general_insert (Node((k,v),empty,empty,empty,empty,0,false)) h in
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
	  Node(skv, p, l, r, general_insert t sc, srk+1, sm)
	else
	  Node(kv, p, l, r, general_insert singlet c, rk+1, m)
    

  let decrease_key = fun _ _ _ -> ()
  let delete_min = fun _ -> None

(*
  let cut = TODO
  let merge = TODO
  let mark = TODO

*)

  let test_insert () = TODO
  let test_decrease_key () = TODO
  let test_delete_min () = TODO
end

