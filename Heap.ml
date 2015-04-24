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

  (* Inserts an element into the heap. Returns updated handle to heap and
   * handle to inserted node. *)
  val insert: key -> value -> heap -> (heap * heap)

  (* Removes the minimum-key element from the heap and returns it along with
   * updated handle to heap. If heap is empty, returns None. *)
  val delete_min: heap -> (key * value) option * heap

  (* Decreases the key of the specified element of the heap. Returns updated
   * handle to heap. *)
  val decrease_key: key -> key -> heap -> heap

  (* Runs all the tests. *)
  val run_tests: unit -> unit

end

module type HEAP_ARG =
sig
  open Order 

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
  type rank = int ref
  type marked = bool ref
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

  (* Returns tree with smaller root key; if keys equal, first arg 
   * returned. Empty heap considered smaller than all non-empty heaps *)
  let minroot (t1: tree) (t2: tree) : tree =
    match t2 with
    | Leaf -> t1
    | Node((k2,_),_,_,_,_,_,_) ->
      match t1 with
      | Leaf -> t2
      | Node((k1,_),_,_,_,_,_,_) ->
	if minkey k1 k2 = k2 then t2 else t1

  let lnk_lst_fold (f: 'a -> heap -> 'a) (acc: 'a) (h: heap) : 'a =
    let rec lnk_lst_fold_helper 
	(f': 'a -> heap -> 'a) (acc': 'a) (h': heap) (h0: heap) : 'a =
      match !h' with
      | Leaf -> acc'
      | Node((k,v),p,l,r,c,rk,m) ->
	if phys_equal l h0
	then f' acc' h'
	else 
	  match !l with
	  | Leaf -> f' acc' h'
	  | _ -> lnk_lst_fold_helper f' (f' acc' h') l h0 in
    match !h with
    | Leaf -> acc
    | Node(_,_,_,r,_,_,_) ->
      match !r with
      | Leaf -> f acc h
      | Node(_,_,l,_,_,_,_) -> lnk_lst_fold_helper f acc h l

  (* Returns smallest root node in heap *)
  let leastroot (h: heap) : tree =
    lnk_lst_fold (fun a h -> minroot a !h) !h h

(* Old implementation of leastroot; delete when finished
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
*)

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

  (* treats a node as orphaned and w/out siblings and inserts into a heap 
   * to the left of the root of the 2nd arg *)
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

  (* cut removes a tree from the surrounding heap. 
   * cut doesn't change parent marked, but it does decrease parent rank.
   * If cut tree has smallest heap node as root, the rest of the tree
   * can be lost unless already referenced elsewhere. *)
  (* NOTE: This is not what cut is supposed to do! Cut should take a non-root
   * node, remove it from its parent and siblings, and MAKE IT A ROOT NODE.
   * Its parent should be marked if it isn't already, and if it is already
   * marked, then the parent should be cut as well. Cut should therefore be
   * recursive. See the MIT spec for more details. *)
  let cut (t: tree) : unit =
    match t with
    | Leaf -> ()
    | Node(kv,p,l,r,c,rk,m) ->
      let clean_siblings : unit =
	(match !l with
	| Leaf -> ()
	| Node(_,_,_,lr,_,_,_) ->
	  lr := !r;
	  (match !r with
	  | Leaf -> failwith "node must be a tree"
	  | Node(_,_,rl,_,_,_,_) ->
	    rl := !l)) in
      let clean_parent : unit =
	(match !p with
	| Leaf -> ()
	| Node(_,_,_,_,pc,prk,_) ->
	  pc := !l; prk := !prk-1) in
      clean_siblings;
      clean_parent

  (* merges orphaned tree w/out siblings w/ other tree, preserves invariants *)
  let merge (single_t: tree) (t: tree) : tree =
    match single_t with
    | Leaf -> t
    | Node(skv,_,_,_,sc,srk,sm) ->
      match t with
      | Leaf -> single_t
      | Node(kv,p,l,r,c,rk,m) ->
	match (minroot single_t t)=single_t with
	| true ->
	  srk := !srk + 1;
	  let newtree = Node(skv,p,l,r,general_insert t sc,srk,sm) in
	  cut t; general_insert newtree r; newtree
	| false -> 
	  rk := !rk + 1;
          Node(kv,p,l,r,general_insert single_t c,rk,m)
	  
(*
  let mark = TODO

*)

  let delete_min (h: heap) : (key * value) option * heap =
    match !h with
    | Leaf -> (None, h)
    | Node((k,v),p,l,r,c,rk,m) ->
      cut !h;
      let temp_h = ref !l in
      lnk_lst_fold (fun () child -> ()(*ignore (general_insert !child temp_h)*)) () c;
      let new_h = ref (leastroot temp_h) in
      let rk_lst : (int * heap) list ref = ref [] in
      let comb_more : bool =
	lnk_lst_fold (fun finished root ->
	  match finished with
	  | true -> true
	  | false ->
	    match !root with
	    | Leaf -> true
	    | Node(rkv,rp,rl,rr,rc,rrk,rm) ->
	      let member = List.fold_left !rk_lst 
		~init:false ~f:(fun b (n,_) -> n = !rrk || b) in
	      (* let compare = (Some (fun (a,_) (b,_) -> a = b)) in
	      match List.Assoc.mem !rk_lst 
		?equal:compare (!rrk,root) with *)
	      match member with
	      | true ->
		(match 
		  (List.fold_left !rk_lst ~init:None 
		     ~f:(fun b (n,h) -> if n = !rrk then Some h else b)) with
		  | None -> failwith "identical rank must exist"
		  | Some eq_rk_heap ->
		(* let eq_rk_tree =
		  List.Assoc.find_exn !rk_lst ?equal:compare (!rrk,root) in *)
		    cut !root; merge !root !eq_rk_heap; rk_lst := [];
	            true)
	      | false -> 
		let new_elt = (!rrk,root) in
		rk_lst := new_elt::!rk_lst; false) false h in
      while comb_more do () done;
      (Some (k,v), new_h)
      

  let decrease_key (big: key) (small: key) (h: heap) : unit = ()

  let test_insert () = TODO
  let test_decrease_key () = TODO
  let test_delete_min () = TODO

  let run_tests () =
    test_insert () ;
    test_decrease_key () ;
    test_delete_min () ;
    ()

end

(* Create a fib heap with (int, string) pairs for testing *)
module IntStringFibHeap = FibonacciHeap(IntStringHeapArg)
(* Uncomment the following when ready to run tests on fib heap *)
(* IntStringFibHeap.run_tests() *)

(* HEAP_ARG for our the Fibonacci Heap representation we will use for our
 * actual algorithm *)
module GeoHeapArg : HEAP_ARG =
struct
  open Order
  (* Keys are distances *)
  type key = float

  (* Values are node names *)
  type value = string
  let compare x y = if x < y then Less else if x > y then Greater else Equal

  (* For testing purposes *)
  let string_of_key = string_of_float
  let string_of_value v = v
  let gen_key () = 0.
  let gen_key_gt x () = x +. 1.
  let gen_key_lt x () = x -. 1.
  let gen_key_between x y () =
    let (lower, higher) = (min x y, max x y) in
    if higher - lower < 2. then None else Some (higher -. 1.)
  let gen_key_random =
    let _ = Random.self_init () in
    (fun () -> Random.float 10000.)

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


(* Our actual fib heap module - not sure if this is where it should go *)
module FibHeap = FibonacciHeap(GeoHeapArg)

(* Our actual graph representation (not sure where to put it either,
 * but it definitely needs to happen after FibHeap is defined because it
 * uses FibHeap in its own definition) *)
(* Nodes consist of a string (the name of the node) and a FibHeap.heap option
 * (a pointer to the corresponding node in the Fibonacci Heap, if this node
 * exists in the heap) *)
module GeoNode =
struct
  type node = {name: string; mutable pt: FibHeap.heap option}
  type weight = float
  let compare n1 n2 = string_compare s1.name s2.name
  let string_of_node n = n.name
  let get () = {name = ""; pt = None}
end

module GeoGraph = Graph(GeoNode)

