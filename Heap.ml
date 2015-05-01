(****************************************
 * Fibonacci Heap module
 * CS51
 ***************************************)
open Core.Std
module Regex = Re2.Regex
open Graph
open Links

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
  (* Checks if heap is empty. *)
  val is_empty: heap -> bool

  (* Inserts an element into the heap. Returns updated handle to heap and
   * handle to inserted node. *)
  val insert: key -> value -> heap -> (heap * heap)

  (* Removes the minimum-key element from the heap and returns it along with
   * updated handle to heap. If heap is empty, returns None. *)
  val delete_min: heap -> (key * value) option * heap

  (* Decreases the key of the specified node of the heap. Returns updated
   * handle to heap. *)
  val decrease_key: heap -> key -> heap -> heap

  (* Returns the key and value associated with a particular node, or None
   * if the node corresponds to an empty heap. *)
  val get_top_node: heap -> (key * value) option

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
    with type key = H.key) =
struct
  type key = H.key
  type value = H.value
  (* A heap will consist of a node option, where a node is
   * a record to key, value, parent heap, left heap, right heap,
   * child heap, no. of children (rank), child cut (marked)) *)
  type node = { mutable k: key; 
	                v: value; 
	        mutable p: heap; 
	        mutable l: heap; 
	        mutable r: heap; 
	        mutable c: heap; 
	        mutable rk: int; 
                mutable mk: bool}
  and heap = (node option) ref

  let empty : heap = ref None

  let is_empty (h: heap) : bool =
    match !h with
    | None -> true
    | _ -> false

(* unused; remove unless needed later

  let minkey (k1: key) (k2: key) : key =
    match H.compare k2 k1 with
    | Less -> k2
    | _ -> k1
*)
  let get_top_node (h: heap) : (key * value) option =
    match !h with
    | None -> None
    | Some n -> Some (n.k,n.v)

  (* Returns heap with smaller root key; if keys equal, first arg 
   * returned. Empty heap considered larger than all non-empty heaps *)
  let minroot (h1: heap) (h2: heap) : heap =
    match get_top_node h1, get_top_node h2 with
    | None, None -> h1
    | Some _, None -> h1
    | None, Some _ -> h2
    | Some (k1,_), Some (k2,_) -> if H.compare k2 k1 = Less then h2 else h1

  (* A fold function across a double linked list of heaps. The function
   * folds left and stops when it's made a full loop *)
  let lnk_lst_fold (f: 'a -> heap -> 'a) (acc: 'a) (h: heap) : 'a =
    let rec lnk_lst_fold_helper (f': 'a -> heap -> 'a) (acc': 'a)
        (h': heap) (h0: heap) : 'a =
      match !h' with
      | None -> acc'
      | Some n ->
        if phys_equal n.l h0
        then f' acc' h'
        else 
          match !(n.l) with
          | None -> f' acc' h'
          | _ -> lnk_lst_fold_helper f' (f' acc' h') n.l h0
    in
    match !h with
    | None -> acc
    | Some n ->
      match !(n.r) with
      | None -> failwith "nodes must have real siblings"
      | Some rn -> lnk_lst_fold_helper f acc h rn.l

  (* Returns smallest root node in heap *)
  let leastroot (h: heap) : heap = lnk_lst_fold minroot h h

  (* reassigns sibling fields to make h1 & h2 left/right siblings. If
   * either arg is None, the other arg is return by itself *)
  let link (hl: heap) (hr: heap) : unit =
    match !hl,!hr with
    | _,None -> ()
    | None,_ -> ()
    | Some nl,Some nr -> nl.r <- hr; nr.l <- hl   

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

  (* NOTE: I don't think we need this anymore, as we discussed yesterday *)
  (* treats a node as orphaned and w/out siblings and inserts into a heap 
   * to the left of the root of the 2nd arg *)
  (*
  let general_insert (new_h: heap) (h: heap) : heap =
    match !h with
    | None -> new_h
    | Some n ->
        (match !new_h with
        | None -> h
        | Some new_n ->
            (match !(n.l) with
            | None -> 
                let newnode = Node((k,v), empty, h, h, c, rk, m) in
                h := Node((hk,hv), hp, ref newnode, ref newnode, hc, hrk, hm);
                ref newnode
            | Some ln ->
                let newnode = Node((k,v), empty, hl, h, c, rk, m) in
                hl := Node(lkv, lp, ll, ref newnode, lc, lrk, lm);
                h := Node((hk,hv), hp, ref newnode, hr, hc, hrk, hm);
                ref newnode))
  *)

  (* given a key, value, and heap, inserts a new node into the root list 
   * of the heap with the containing the key value pair and returns the
   * updated pointer to the min as well as a pointer to the new node *)
  let insert (k: key) (v: value) (h: heap) : heap * heap =
    match !h with
    | None ->
        (* If h is empty, then the new node's siblings should be itself *)
        let newheap = empty in
        let newnode =
          {k=k;v=v;p=empty;l=newheap;r=newheap;c=empty;rk=0;mk=false}
        in
        newheap := Some newnode; (newheap,newheap)
    | Some n ->
        (* If h is not empty, then insert new node to left of current min *)
        let newnode = {k=k;v=v;p=empty;l=n.l;r=h;c=empty;rk=0;mk=false} in
        let newheap = ref (Some newnode) in
        match !(n.l) with
        | None -> failwith "Node must have real siblings"
        | Some l ->
            n.l <- newheap; l.r <- newheap; ((minroot h newheap),newheap)
 
  (* clean removes a tree from the surrounding heap. 
   * clean doesn't change parent marked, but it does decrease parent rank.
   * If cleaned tree has smallest heap node as root, the rest of the tree
   * can be lost unless already referenced elsewhere. *)
  let clean (h: heap) : unit =
    match !h with
    | None -> ()
    | Some n ->
        let clean_siblings : unit =
          match !(n.l),!(n.r) with
          | Some _, Some _ -> link n.l n.r
          | _,_ -> failwith "node must have real siblings"
        in
        let clean_parent : unit =
          match !(n.p) with
          | None -> ()
          | Some p ->
              if p.rk = 1 then p.c <- empty else p.c <- n.l; 
              p.rk <- p.rk-1
        in
        clean_siblings;
        clean_parent

  (* merges two heaps by making larger-key root a child of smaller-key root *)
  let rec merge (h1: heap) (h2: heap) : unit =
    match !h1,!h2 with
    | _, None -> ()
    | None, _ -> ()
    | Some n1, Some n2 ->
        match H.compare n1.k n2.k with
        | Less | Equal ->
            n1.rk <- n1.rk + 1; clean h2;
            (match !(n1.c) with
            (* If minroot has no children, now it has other root as child *)
            | None -> n1.c <- h2
            (* If minroot already had children, other root
             * inserted to the left of the referenced child *)
            | Some cn -> 
                let lh = cn.l in 
                link lh h2; link h2 n1.c)
    | _ -> merge h2 h1

  (* Deletes the minimum element from the heap and returns it along with an
   * updated handle to the heap. *)
  let delete_min (h: heap) : (key * value) option * heap =
    match !h with
    | None -> (None, h)
    | Some n ->
      let insert_children (h': heap) : unit =
        (match !h' with
        | None -> ()
        | Some n' -> 
            lnk_lst_fold 
            (fun () c -> 
              match !c with
              | None -> failwith "node cannot be empty"
              | Some cn ->
                  link n.l c; link c h; cn.p <- empty) () n'.c; n'.rk <- 0)
      in
      insert_children h;
      let l = n.l in clean h; 
      let nh = leastroot l in
      let rk_lst : heap list ref = ref [] in
      (* try to merge a heap with any heap in rk_lst *)
      let try_merge (h': heap) : bool =
	let merged_once = List.fold_left !rk_lst ~init:false
	  ~f:(fun merged comp_h -> 
	    if merged then merged else
	      match !comp_h, !h with
	      | None,_ | _,None -> failwith "node cannot be empty"
	      | Some comp_n, Some n' ->
		if comp_n.rk = n'.rk
		then 
		  let _ = merge comp_h h'; rk_lst := [] in 
		  true
		else 
		  false) in
	if merged_once 
	then true 
	else (let _ = rk_lst := h'::!rk_lst in false)
      in
      (* recurse through lnk list w/ merged_once until it merges once only *)
      let merge_more (h': heap) : bool =
        lnk_lst_fold (fun merged h ->
          if merged then merged else try_merge h) false h' in
            while merge_more h do () done;
            (Some (n.k, n.v), nh)
      
(* Bits of old code from delete_min; delete when done

        let rk_lst : (int * heap) list ref = ref [] in
        let comb_more : bool =
          lnk_lst_fold (fun finished root ->
            if finished then true else
                (match !root with
                | Leaf -> true
                | Node(rkv,rp,rl,rr,rc,rrk,rm) ->
                    let member = List.fold_left !rk_lst ~init:false
                        ~f:(fun b (n,_) -> n = !rrk || b) in
                  (* let compare = (Some (fun (a,_) (b,_) -> a = b)) in
                  match List.Assoc.mem !rk_lst 
              ?equal:compare (!rrk,root) with *)
                    match member with
                    | true ->
                        (match (List.fold_left !rk_lst ~init:None 
                             ~f:(fun b (n,h) -> if n = !rrk then Some h
                              else b)) with
                        | None -> failwith "identical rank must exist"
                        | Some eq_rk_heap ->
                        (* let eq_rk_tree =
                          List.Assoc.find_exn !rk_lst ?equal:compare (!rrk,root) in *)
                            cut !root; merge !root !eq_rk_heap;
                            rk_lst := []; true)
                    | false -> 
                        let new_elt = (!rrk,root) in
                        rk_lst := new_elt::!rk_lst; false) false h in
                        while comb_more do () done;
                        (Some (k,v), new_h)
*)                         

  (* Cut detaches a node from its parent & siblings and adds it to the root
   * list, then recursively cuts node parents that are marked until it 
   * reaches an unmarked node, returning an updated handle to the heap. *)
  let rec cut (h: heap) (top: heap) : heap =
    match !h with
    | None -> failwith "Unable to cut an empty heap"
    | Some n ->
      let ph = n.p in
      match !ph with
      | None -> top
      | Some pn ->
          match !top with
          | None -> failwith "Unable to cut an empty heap"
          | Some tn ->
              let _ = clean h; n.p <- empty; link tn.l h; link h top in
              let newtop = minroot top h in
              if pn.mk then cut ph newtop else
                if !(pn.p) = None 
                then newtop
                else let _ = pn.mk <- true in newtop
      
(* Leftovers from updates to cut function; may be useful; delete when done

        (match !(n.p) with
        (* If node is already a root, we don't have to do anything *)
        | None -> top
        | Some pn ->
            (match !(n.l),!(n.r) with
            | None,_ | _,None -> failwith "siblings should never be empty"
            | Some ln, Some rn ->
	      
                let _ = ln.r <- n.r ; rn.l <- n.l ;
                  n.p <- (ref None) in
                (* If node's siblings are the same as itself, it has no
                 * real siblings -  after cut parent has no children *)
                let _ = if (phys_equal n.l nd) then par.ch <- None
                  else par.ch <- n.l in
                let _ = n.l <- t.l ; n.r <- top ; t.l <- nd in
                if par.mk then cut n.p (minroot top nd)
                else minroot top nd)))
*)

  (* Decreases key of existing node; cuts the node if heap ordering is
   * violated. *)
  let decrease_key (h: heap) (small: key) (t: heap) : heap =
    match !h with
    | None -> failwith "Cannot decrease key of an empty heap"
    | Some n ->
        assert((H.compare small n.k) = Less) ;
        match get_top_node n.p with
        (* If parent is empty, this must be a root node already *)
        | None -> n.k <- small; minroot t h
        | Some (pk,_) ->
            n.k <- small;
            match H.compare pk small with
            (* If parent key is still smaller or equal, heap ordering is
             * fine and we just update without changing anything else *)
            | Less | Equal -> t
            | Greater -> cut n.p t

  (*****************************)
  (***** Testing Functions *****)
  (*****************************)

  (* Finds number of nodes inside a Fibonacci heap *)
  let rec num_nodes (h: heap) : int =
    lnk_lst_fold (fun a h' ->
      match !h' with
      | None -> a
      | Some n ->
	a + 1 + (num_nodes n.c) ) 0 h

  (* Inserts a list of pairs into the given heap and returns a handle to the
   * resulting heap as well as a list of nodes corresponding to each pair,
   * in the same order as the original pair list it corresponds to. *)
  let insert_list (h: heap) (lst: (key * value) list) : heap * heap list =
    let insert_keep_track (k,v) r =
      let (sofar,hs) = r in
      let (whole,mine) = insert k v sofar in 
      assert ((num_nodes sofar) + 1 = (num_nodes whole));
      (whole, mine::hs)
    in
    List.fold_right lst ~f:insert_keep_track ~init:(h,[])

  (* Generates a (key,value) list with n distinct keys in increasing order,
   * starting from a given key. *)
  let rec gen_pairs_from (size: int) (current: key) : (key * value) list =
    if size <= 0 then []
    else
      let new_current = H.gen_key_gt current () in
      (new_current, H.gen_value()) :: (gen_pairs_from (size - 1) new_current)

  (* Generates a (key,value) list with n distinct keys in increasing order. *)
  let generate_pair_list (size: int) : (key * value) list =
    gen_pairs_from size (H.gen_key ())

  (* Generates a (key,value) list with keys in random order. *)
  let rec generate_random_list (size: int) : (key * value) list =
    if size <= 0 then []
    else
      (H.gen_key_random(), H.gen_value()) :: (generate_random_list (size - 1))

  (* Generates a (key,value) list with identical keys. *)
  let rec generate_identical_list (k: key) (size: int) : (key * value) list =
    if size <= 0 then []
    else
      (k, H.gen_value()) :: (generate_identical_list k (size - 1))

  (* Returns the minimum of a list of pairs. If multiple pairs have the same 
   * key, returns the first of the pairs. *)
  let min_pair (lst: (key * value) list) : (key * value) option =
    let rec min_helper lst curr =
      match lst with
      | [] -> curr
      | (k,v)::tl ->
          (match curr with
          | None -> min_helper tl (Some (k,v))
          | Some (currk,currv) ->
              (match H.compare k currk with
              | Less -> min_helper tl (Some (k,v))
              | Greater | Equal -> min_helper tl (Some (currk,currv))))
    in
    min_helper lst None

  let top_matches (a: (key * value)) (pt: heap) : bool =
    match get_top_node pt with
    | None -> false
    | Some b -> a = b
    
  let test_insert () = 
    (* Fill heap with random pairs *)
    let randpairs = generate_random_list 100 in
    let (h1,lst1) = insert_list empty randpairs in
    (* Check that every pair is where insert said it was *)
    List.iter2_exn ~f:(fun a pt -> assert(top_matches a pt)) randpairs lst1 ;
    (* Check that are 100 nodes in the heap *)
    assert((num_nodes h1) = 100) ;
    (* Check that the minimum pair ended up in the min spot *)
    assert((min_pair randpairs) = (get_top_node h1)) ;
    (* Rinse and repeat with a sequential list of pairs *)
    let seqpairs = generate_pair_list 100 in
    let (h2,lst2) = insert_list empty seqpairs in
    List.iter2_exn ~f:(fun a pt -> assert(top_matches a pt)) seqpairs lst2 ;
    assert((List.hd seqpairs) = (get_top_node h2)) ;
    (* Rinse and repeat with a reverse-sequential list *)
    let revpairs = List.rev seqpairs in
    let (h3,lst3) = insert_list empty revpairs in
    List.iter2_exn ~f:(fun a pt -> assert(top_matches a pt)) revpairs lst3 ;
    assert((List.hd seqpairs) = (get_top_node h3)) ;
    ()

  let test_decrease_key () =
    (* Fill heap with identical pairs *)
    let key1 = H.gen_key() in
    let identpairs = generate_identical_list key1 100 in
    let (idheap, idlist) = insert_list empty identpairs in
    let (id1,id2,ide) = match idlist with
      | [] -> failwith "list can't be empty"
      | id1::id2::id3::_ -> id1,id2,id3
      | _ -> failwith "list must have 100 nodes" in
    let key0 = (H.gen_key_lt (H.gen_key()) ()) in
    let heap1 = decrease_key id1 key0 idheap in
    assert(Some (key0, H.gen_value()) = get_top_node heap1) ;
    let keymid = match H.gen_key_between key0 key1 () with
      | None -> failwith "not possible"
      | Some keymid -> keymid in
    let heap2 = decrease_key id2 keymid heap1 in
    assert(Some (key0, H.gen_value()) = get_top_node heap2) ;
    let seqpairs = generate_pair_list 100 in
    let (seqheap, seqlst) = insert_list empty seqpairs in
    let seqlst' = match seqlst with
      | [] -> failwith "list is not empty"
      | _::tl -> tl in
    let (_,seqheap') = delete_min seqheap in
    let seqheap'' = List.fold_left ~f:(fun t h ->
      match !h with
      | None -> failwith "all nodes are real"
      | Some n ->
	match !(n.p) with
	| None -> let nh = decrease_key h (H.gen_key_lt (n.k) ()) t in
		  assert(!(n.p) = None) ; nh
	| Some p -> let nh = decrease_key h (H.gen_key_lt (p.k) ()) t in
		    assert(!(n.p) = None) ; nh) ~init:seqheap' seqlst'
    in
    assert((num_nodes seqheap'') = (num_nodes seqheap')) ;
    ()
    
  let test_delete_min () = () (* TODO *)

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
module GeoHeapArg : (HEAP_ARG with type key = float with type value = string) =
struct
  open Order
  (* Keys are distances *)
  type key = float

  (* Values are node names *)
  type value = string
  let compare x y = if x < y then Less else if x > y then Greater else Equal

  (* For testing purposes *)
  let string_of_key = Float.to_string
  let string_of_value v = v
  let gen_key () = 0.
  let gen_key_gt x () = x +. 1.
  let gen_key_lt x () = x -. 1.
  let gen_key_between x y () =
    let (lower, higher) = (min x y, max x y) in
    if higher -. lower < 2. then None else Some (higher -. 1.)
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

module FibHeap : (PRIOHEAP with type value = GeoHeapArg.value
    with type key = GeoHeapArg.key) = FibonacciHeap(GeoHeapArg) 

(* Our actual node & graph representations (not sure where to put these either,
 * but it definitely needs to happen after FibHeap is defined because it
 * uses FibHeap in its own definition) *)
(* Nodes consist of a string (the name of the node), a FibHeap.heap option
 * (a pointer to the corresponding node in the Fibonacci Heap, if this node
 * exists in the heap), and a pointer to the previous node in the MST once
 * Dijkstra's algorithm has reached this node. *)

type node_record = {name: string; mutable pt: FibHeap.heap option;
    mutable prev: node_record link}

module NodeBase : (NODE with type tag = string with type weight = float
    with type node = node_record) =
struct
  type weight = float
  type tag = string
  type node = node_record
  let tag_of_node = (fun n -> n.name)
  let node_of_tag t = {name = t; pt = None; prev = Nil}
  let compare n1 n2 = 
    let ord = String.compare n1.name n2.name in
    if ord < 0 then Less else
      if ord = 0 then Equal
      else Greater
  let string_of_node n = n.name
  let gen () = {name = ""; pt = None; prev = Nil}
  let gen_weight () = 0.0
  let string_of_weight = Float.to_string
end

module GeoNode =
struct
  include NodeBase

  let set_node_pt (n: node) (p: FibHeap.heap option) : unit = n.pt <- p
  let set_node_prev (n: node) (p: node link) : unit = n.prev <- p

  let get_node_pt (n: node) : FibHeap.heap option = n.pt
  let get_node_prev (n: node) : node link = n.prev
end

module GeoGraph : (GRAPH with type node = GeoNode.node
    with type weight = GeoNode.weight with type tag = GeoNode.tag) = 
    Graph(GeoNode);;

(* code for reading in the csv file *)
let addedges (castlist: (string * (float * float)) list) 
(graph: GeoGraph.graph) (cutoff: float) : GeoGraph.graph = 
  let rec addhelper (place: string * (float * float)) 
  (rest: (string * (float * float)) list) (graph: GeoGraph.graph)
  : GeoGraph.graph = 
    match rest with
    | [] -> graph
    | (name2, loc2)::tl ->
        (let (name1,loc1) = place in
        match Distance.distance cutoff loc1 loc2 with
        | None -> addhelper place tl graph
        | Some d ->
            let newgraph = GeoGraph.add_edge graph
                          (GeoNode.node_of_tag name1)
                          (GeoNode.node_of_tag name2) d 
            in
            addhelper place tl newgraph)
  in
  match castlist with
  | [] -> graph
  | hd::tl -> addhelper hd tl graph

let read_csv () : GeoGraph.graph =  
  let usage () = Printf.printf "usage: %s csv cutoff\n" Sys.argv.(0); exit 1 in 
  if Array.length Sys.argv <> 3 then usage () ;
  (* file pointer - just like in CS50! *)
  let cutoff = Float.of_string (Sys.argv.(2)) in
  let delimiter = Regex.create_exn ";" in
  let parse_line line = Regex.split delimiter line in
  let parsed = In_channel.with_file Sys.argv.(1)
               ~f:(fun file -> In_channel.fold_lines file ~init:[]
               ~f:(fun lst ln -> (parse_line ln)::lst))
  in
  (*let file = In_channel.create Sys.argv.(1) in
  let lines = In_channel.input_lines file in*)
  match parsed with
  | [] -> failwith "CSV file empty or could not be read"
  | []::_ -> failwith "CSV file should not start with empty line"
  | (hd::_)::_ ->
      let _ = Printf.printf "%s\n" hd in
      let rec cast (parselist: string list list) :
          (string * (float * float)) list =
         match parselist with
         | [] -> []  
         | hd::tl ->
             (match hd with 
             | [name;lat;lng] ->
               (name,((Float.of_string lat),(Float.of_string lng)))::(cast tl)
             | _ -> [])
      in
      let casted = cast parsed in
      addedges casted GeoGraph.empty cutoff






(* Request start and finish nodes from user *)
let rec get_nodes (g: GeoGraph.graph) : GeoNode.node * GeoNode.node =
  (* get_nodes should actually return an option GeoNode.node * GeoNode.node *) 
  (* Should give the user a text prompt so they know what to input *)
  let () = Printf.printf "Starting Point: " in
  let st = read_line () in
  let stnode = GeoNode.node_of_tag st in
  if (not (GeoGraph.has_node g stnode)) then
    let () = Printf.printf "Node is not in graph. Try again.\n" in
    get_nodes g
  else 
    let () = Printf.printf "End Point: " in
    let fin = read_line () in
    let finnode = GeoNode.node_of_tag fin in
    if (not (GeoGraph.has_node g finnode)) then
      let () = Printf.printf "Node is not in graph. Try again.\n" in
      get_nodes g
    else (stnode, finnode) ;;

(* Run dijkstra's algorithm to find shortest path between start and finish *)
let dijkstra (st: GeoNode.node) (fin: GeoNode.node) (g: GeoGraph.graph)
    : (GeoNode.node list) * GeoNode.weight =
  
  (* Initialize heap containing only source node with distance of 0 *)

  let (with_source,_) =
    FibHeap.insert 0. (GeoNode.tag_of_node st) FibHeap.empty
  in
  (* Insert all other nodes into heap with initial distance of infinity (using
   * Float.max_value to represent infinity) and hold on to a pointer to each *)
  let insert_not_source (h: FibHeap.heap) (s: GeoNode.node) : FibHeap.heap =
    match (GeoNode.compare s st) with
    | Less | Greater -> 
        let (hp,nd) =
          FibHeap.insert Float.max_value (GeoNode.tag_of_node s) h
        in
        let _ = GeoNode.set_node_pt s (Some nd) in hp
    | Equal -> h
  in
  let fib_heap = List.fold_left (GeoGraph.nodes g) ~f:insert_not_source
      ~init:with_source in

  (* Keep taking min, checking its neighbors, and updating distances until
   * our destination node is the min that we take. *)
  let rec next_node (h: FibHeap.heap) (prev: GeoNode.node Links.link) =
    let (min,hp) = FibHeap.delete_min h in
    match min with
    | None -> failwith "heap empty without reaching destination"
    | Some (dist,nm) ->
        (* Find corresponding node in graph *)
        (match GeoGraph.get_node_by_tag g nm with
        | None -> failwith "heap min is not in graph"
        | Some this_nd ->
            (* Record that we've visited this node *)
            let _ = GeoNode.set_node_pt this_nd None in
            (* Record where we've been *)
            let _ = GeoNode.set_node_prev this_nd prev in
            (* If the min node we pulled was our destination, we're done;
             * return distance and list of nodes in the shortest path *)
            (match GeoNode.compare this_nd fin with
            | Equal ->
                (Links.list_of_links (Node (this_nd,this_nd.prev)),dist)
            | Less | Greater ->
                (* Otherwise, get the neighbors of our min *)
                (match GeoGraph.neighbors g this_nd with
                | None -> failwith "we already checked that this_node exists"
                | Some ns ->
                    (* For each neighbor, update distance if necessary *)
                    let handle_node (h: FibHeap.heap) (n,w) =
                      let alt = dist +. w in
                      (* If no heap pointer associated with this node, we must
                       * have visited it already, so don't decrease any key *)
                      (match n.pt with
                      | None -> h
                      | Some pnt ->
                          (* Otherwise, decrease key of appropriate node *)
                          (match FibHeap.get_top_node pnt with
                          | None -> failwith "empty heap node"
                          | Some (k,v) -> assert(v = n.name) ;
                              if alt < k then FibHeap.decrease_key pnt alt h
                              else h))
                    in
                    next_node (List.fold_left ns ~f:handle_node ~init:hp)
                        (Link (ref (Node (this_nd,prev))) ))))
  in next_node fib_heap Nil ;;

let graph = read_csv () in
let (start,finish) = get_nodes graph in
let (nodelist, weight) = dijkstra start finish graph in
let rec printnodes (lst: GeoNode.node list) : unit = 
  match lst with 
  | [] -> ()
  | hd::tl -> Printf.printf "%s ->\n" (GeoNode.tag_of_node hd) ; printnodes tl
in
printnodes nodelist; Printf.printf "\nTotal distance: %f" weight; ()


