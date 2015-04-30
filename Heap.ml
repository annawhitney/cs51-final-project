(****************************************
 * Fibonacci Heap module
 * CS51
 ***************************************)
open Core.Std
open Graph

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
  val insert: key -> value -> heap option -> (heap * heap)

  (* Removes the minimum-key element from the heap and returns it along with
   * updated handle to heap. If heap is empty, returns None. *)
  val delete_min: heap -> (key * value) option * heap

  (* Decreases the key of the specified node of the heap. Returns updated
   * handle to heap. *)
  val decrease_key: heap -> key -> heap -> heap

  (* Returns the key and value associated with a particular node. *)
  val get_top_node: heap -> (key * value)

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
  (* NOTE: I switched the ref and option parts of the definition; I know
   * we decided otherwise but on second look I noticed that it actually
   * makes things much cleaner. *)
  (* A heap will consist of a node option, where a node is
   * a record to key, value, parent heap, left heap, right heap,
   * child heap, no. of children (rank), child cut (marked)) *)
  type node = { mutable k: key; 
	                v: value; 
	        mutable p: heap option; 
	        mutable l: heap; 
	        mutable r: heap; 
	        mutable c: heap option; 
	        mutable rk: int; 
		mutable mk: bool}
  and heap = node ref

  let empty : heap option = None

  let is_empty (h: heap) : bool =
    match h with
    | None -> true
    | _ -> false

  let minkey (k1: key) (k2: key) : key =
    match H.compare k2 k1 with
    | Less -> k2
    | _ -> k1

  let get_top_node (h: heap) : key * value = (h.k,h.v)

  (* Returns heap with smaller root key; if keys equal, first arg returned. *)
  let minroot (h1: heap) (h2: heap) : heap =
    let (k1,v1) = get_top_node h1 in
    let (k2,v2) = get_top_node h2 in
    if minkey k1 k2 = k2 then h2 else h1

  let lnk_lst_fold (f: 'a -> heap -> 'a) (acc: 'a) (h: heap) : 'a =
    let rec lnk_lst_fold_helper (f': 'a -> heap -> 'a) (acc': 'a)
        (h': heap) (h0: heap) : 'a =
      let n = !h' in
      if phys_equal n.l h0
      then f' acc' h'
      else lnk_lst_fold_helper f' (f' acc' h') n.l h0 in
    let n = !h in
    let rn = !(n.r) in
    lnk_lst_fold_helper f acc h rn.l

  (* Returns smallest root node in heap *)
  let leastroot (h: heap) : heap = lnk_lst_fold minroot h h

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
  let insert (k: key) (v: value) (h_op: heap option) : heap * heap =
    match h_op with
    | None ->
        (* If h is empty, then the new node's siblings should be itself *)
        let newnode =
	  {k=k;v=v;p=empty;l=newheap;r=newheap;ch=empty;rk=0;mk=false} in
        l <- newnode; r <- newnode; (newheap,newheap)
    | Some h ->
        (* If h_op is not empty, then insert new node to left of current min *)
        let newnode = {k=k;v=v;p=empty;l=h.l;r=h;ch=empty;rk=0;mk=false} in
	let {l=l} = newnode in
	h.l <- newheap; l.r <- newheap; ((minroot h newheap),newheap)

  (* clean removes a tree from the surrounding heap. 
   * clean doesn't change parent marked, but it does decrease parent rank.
   * If cleaned tree has smallest heap node as root, the rest of the tree
   * can be lost unless already referenced elsewhere. *)
  let clean (h: heap) : unit =
    let clean_siblings : unit =
      let l = h.l in l.r <- !(h.r);
      let r = h.r in r.l <- !(h.l) in
    let clean_parent : unit =
      match h.p with
      | None -> ()
      | Some p -> 
	match (!p).c with
	| None -> failwith "parent node must have child"
	| Some pc -> pc <- !(h.l) in
    clean_siblings;
    clean_parent

  (* merges two heaps by making larger-key root a child of smaller-key root *)
  let merge (h1: heap) (h2: heap) : heap =
    match H.compare h1 h2 with
    | Less -> 
    | _ -> 
    if (minroot h1 h2) = h1 then
              n1.rk <- n1.rk + 1 ;
              match !(n1.ch) with
              (* If minroot has no children, now it has other root as child *)
              | None -> n1.ch <- h2 ; h1
              (* If minroot already had children, other root inserted to the
               * left of the child the minroot has reference to *)
              | Some chn -> n2.l <- chn.l ; n2.r <- n1.ch ; chn.l <- h2 ; h1
            else
              n2.rk <- n2.rk + 1;
              match !(n2.ch) with
              | None -> n2.ch <- h1 ; h2
              | Some chn -> n1.l <- chn.l ; n1.r <- n2.ch ; chn.l <- h1 ; h2)


  (* Deletes the minimum element from the heap and returns it along with an
   * updated handle to the heap. *)
  let delete_min (h: heap) : (key * value) option * heap =
    match !h with
    | None -> (None, h)
    | Some n ->
        cut !h;
        let temp_h = ref !l in
        lnk_lst_fold (fun () child -> ()(*ignore (general_insert !child temp_h)*)) () c;
        let new_h = ref (leastroot temp_h) in
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
                          
  (* Cut detaches a node from its parent & siblings and adds it to the root
   * list, returning an updated handle to the heap. *)
  let rec cut (n: heap) (top: heap) : heap =
    match !n with
    | None -> failwith "shouldn't be trying to cut an empty heap"
    | Some n ->
        (match !(n.p) with
        (* If node is already a root, we don't have to do anything *)
        | None -> top
        | Some par ->
            (match !(n.l) with
            | None -> failwith "siblings should never be empty"
            | Some left ->
                (match !(n.r) with
                | None -> failwith "siblings should never be empty"
                | Some right ->
                    (match !top with
                    | None -> failwith "can't cut from empty heap"
                    | Some t ->
                        let _ = left.r <- n.r ; right.l <- n.l ;
                            n.p <- (ref None) in
                        (* If node's siblings are the same as itself, it has no
                         * real siblings -  after cut parent has no children *)
                        let _ = if (phys_equal n.l nd) then par.ch <- None
                        else par.ch <- n.l in
                        let _ = n.l <- t.l ; n.r <- top ; t.l <- nd in
                        if par.mk then cut n.p (minroot top nd)
                        else minroot top nd))))

  (* Decreases key of existing node; cuts the node if heap ordering is
   * violated. *)
  let decrease_key (nd: heap) (small: key) (h: heap) : heap =
    match !nd with
    | None -> failwith "shouldn't be trying to decrease key of an empty heap"
    | Some n ->
        assert((H.compare small n.k) = Less) ;
        (match get_top_node n.p with
        (* If parent is a Leaf, this must be a root node already *)
        | None -> let _ = n.k <- small in (minroot h nd)
        | Some (k,_) ->
            (match H.compare k small with
            (* If parent key is still smaller or equal, heap ordering is fine 
             * and we just update without changing anything else *)
            | Less | Equal -> let _ = n.k <- small in h
            | Greater -> let _ = n.k <- small in cut nd h

  (*****************************)
  (***** Testing Functions *****)
  (*****************************)

  (* Inserts a list of pairs into the given heap and returns a handle to the
   * resulting heap as well as a list of nodes corresponding to each pair,
   * in the same order as the original pair list it corresponds to. *)
  let insert_list (h: heap) (lst: (key * value) list) : heap * heap list =
    let insert_keep_track (k,v) r =
      let (sofar,hs) = r in
      let (whole,mine) = insert k v sofar in (whole, mine::hs)
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
  let rec generate_identical_list (size: int) : (key * value) list =
    if size <= 0 then []
    else
      (H.gen_key(), H.gen_value()) :: (generate_identical_list (size - 1))

  (* Returns the minimum of a list of pairs. If multiple pairs have the same 
   * key, returns the first of the pairs. *)
  let min_pair (lst: pair list) : pair option =
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


  let test_insert () = 
    let top_matches (a: pair) (pt: heap) : bool =
      match get_top_node pt with
      | None -> false
      | Some b -> a = b
    in
    (* Fill heap with random pairs *)
    let randpairs = generate_random_list 100 in
    let (h1,lst1) = insert_list empty randpairs in
    (* Check that every pair is where insert said it was *)
    List.iter2_exn ~f:(fun a pt -> assert(top_matches a pt)) randpairs lst1 ;
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

  let test_decrease_key () = () (* TODO *)
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

(* Our actual node & graph representations (not sure where to put these either,
 * but it definitely needs to happen after FibHeap is defined because it
 * uses FibHeap in its own definition) *)
(* Nodes consist of a string (the name of the node), a FibHeap.heap option
 * (a pointer to the corresponding node in the Fibonacci Heap, if this node
 * exists in the heap), and a pointer to the previous node in the MST once
 * Dijkstra's algorithm has reached this node. *)
module GeoNode : NODE =
struct
  type node = {name: string; mutable pt: FibHeap.heap option;
      mutable prev: node ref option}
  type weight = float
  type tag = string
  let tag_of_node n = n.name
  (*let node_of_tag t = {name: t; pt = None; prev = None}*)
  let compare n1 n2 = string_compare s1.name s2.name
  let string_of_node n = n.name
  let get () = {name = ""; pt = None; prev = None}
end
(*
module GeoGraph = Graph(GeoNode)
*)
