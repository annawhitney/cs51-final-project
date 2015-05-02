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
module IntStringHeapArg : (HEAP_ARG with type key = int
    with type value = string) =
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

  (* The empty heap. *)
  let empty : heap = ref None

  let is_empty (h: heap) : bool =
    match !h with
    | None -> true
    | _ -> false

  (* Returns the key,value pair associated with a given node of the heap. *)
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

  (* A fold function across a doubly linked list of heaps. The function
   * folds right and stops when it has made a full loop. *)
  let lnk_lst_fold (f: 'a -> heap -> 'a) (acc: 'a) (h: heap) : 'a =
    let rec lnk_lst_fold_helper (f: 'a -> heap -> 'a) (acc: 'a)
        (h: heap) (h0: heap) : 'a =
      match !h with
      | None -> acc
      | Some n ->
        if phys_equal n.r h0
        then f acc h
        else lnk_lst_fold_helper f (f acc h) n.r h0
    in
    lnk_lst_fold_helper f acc h h
      
  (* Returns smallest root node in heap. *)
  let leastroot (h: heap) : heap = lnk_lst_fold minroot h h
    
  (* Reassigns sibling fields to make h1 & h2 left/right siblings. If
   * either arg is None, nothing happens. *)
  let link (hl: heap) (hr: heap) : unit =
    match !hl,!hr with
    | _,None -> ()
    | None,_ -> ()
    | Some nl,Some nr -> nl.r <- hr; nr.l <- hl   

  (* given a key, value, and heap, inserts a new node into the root list 
   * of the heap with the containing the key value pair and returns the
   * updated pointer to the min as well as a pointer to the new node *)
  let insert (k: key) (v: value) (h: heap) : heap * heap =
    match !h with
    | None ->
        (* If h is empty, then the new node's siblings should be itself *)
        let newheap = ref None in
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
          n.l <- newheap; 
	  l.r <- newheap; 
	  ((minroot h newheap),newheap)
 
  (* Clean removes a tree from the surrounding heap. 
   * Clean doesn't change parent marked, but it does decrease parent rank.
   * If cleaned tree has smallest heap node as root, the rest of the tree
   * can be lost unless already referenced elsewhere. *)
  let clean (h: heap) : unit =
    match !h with
    | None -> ()
    | Some n ->
      let clean_siblings : unit = link n.l n.r in
      let clean_parent : unit =
        match !(n.p) with
        | None -> ()
        | Some p ->
          p.rk <- p.rk-1;
          if p.rk = 0 then p.c <- empty else p.c <- n.l; 
      in
      clean_siblings;
      clean_parent

  (* Merges two heaps by making larger-key root a child of smaller-key root. *)
  let rec merge (h1: heap) (h2: heap) : unit =
    match !h1,!h2 with
    | _, None | None, _ -> ()
    | Some n1, Some n2 ->
        (match H.compare n1.k n2.k with
        | Less | Equal ->
            n1.rk <- n1.rk + 1; clean h2; n2.p <- h1;
            (match !(n1.c) with
            (* If minroot has no children, now it has other root as child *)
            | None -> n1.c <- h2 ; link h2 h2 ;
            (* If minroot already had children, other root
             * inserted to the left of the referenced child *)
            | Some cn -> 
                let lh = cn.l in 
                link lh h2; link h2 n1.c)
        | _ -> merge h2 h1)

  (* Deletes the minimum element from the heap and returns it along with an
   * updated handle to the heap. *)
  let delete_min (h: heap) : (key * value) option * heap =
    match !h with
    | None -> (None, h)
    | Some n ->
(*        let l = n.l in
        if phys_equal l h then (Some (n.k,n.v),empty)
        else
          let _ = link l n.r in
          let startmin = leastroot l in
          let finalmin = lnk_lst_fold
              (fun min c -> match !c with
                            | None -> failwith "no empty siblings allowed"
                            | Some nd -> nd.p <- empty ;
                                (match !min with
                                | None -> failwith "heap isn't empty"
                                | Some m -> link c m.r ; link min c ;
                                    minroot min c))
              startmin n.c
          in
          (* The rank is O(log n) for a heap of size n, so throwing out a
           * random reasonable (overly high for safety) value... *)
          let max_rank = 200 in
          let ranks : heap option array = Array.create ~len:max_rank None in
          (* Merge pairs of heaps of same rank; keep doing so until no more
           * pairs of same rank exist (i.e., we get all the way around the
           * root list without encountering any two heaps of same rank) *)
          let rec merge_if_necessary (h: heap) (h0: heap) : unit =
            match !h with
            | None -> ()
            | Some n -> if phys_equal n.r h0 then () else
                (match ranks.(n.rk) with
                | None -> ranks.(n.rk) <- Some h ; merge_if_necessary n.r h0
                | Some hr -> merge h hr ;
                    Array.fill ranks ~pos:0 ~len:max_rank None ;
                    merge_if_necessary h0 h0)
          in
          merge_if_necessary finalmin finalmin ; (Some (n.k,n.v),finalmin)
	    
<<<<<<< HEAD
        
*)     
     
      let insert_children (h': heap) : unit =
        (match !h' with
        | None -> ()
        | Some n' -> 
          lnk_lst_fold 
            (fun () c -> 
              match !c with
              | None -> failwith "node cannot be empty"
              | Some cn ->
                link n'.l c; link c h; cn.p <- empty) () n'.c; n'.rk <- 0)
      in
      insert_children h;
      let l = n.l in clean h; 
      let nh = (if phys_equal l h then empty else leastroot l) in
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
		  false)
        in
        if merged_once 
        then true 
        else (let _ = rk_lst := h'::!rk_lst in false)
      in
      (* recurse through lnk list w/ merged_once until it merges once only *)
      let merge_more (h': heap) : bool =
        lnk_lst_fold (fun merged h ->
          if merged then merged else try_merge h) false h' in
      let rec merge_finish (h': heap) : unit =
      	if merge_more h'
      	then merge_finish h'
      	else () in
      (*merge_finish nh;*)
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

=======
>>>>>>> 57c090b6be71b2ac6595760c8e32243b74041cba
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
      
  (* Decreases key of existing node; cuts the node if heap ordering is
   * violated. *)
  let decrease_key (h: heap) (small: key) (t: heap) : heap =
    match !h with
    | None -> failwith "Cannot decrease key of an empty heap"
    | Some n ->
      assert((H.compare small n.k) = Less) ;
      n.k <- small;
      match get_top_node n.p with
      (* If parent is empty, this must be a root node already *)
      | None -> minroot t h
      | Some (pk,_) ->
        match H.compare pk small with
        (* If parent key is still smaller or equal, heap ordering is
         * fine and we just update without changing anything else *)
        | Less | Equal -> t
        | Greater -> cut n.p t

  (*****************************)
  (***** Testing Functions *****)
  (*****************************)

  let rec num_nodes (h: heap) : int =
    lnk_lst_fold (fun a h' ->
      match !h' with
      | None -> failwith "empty heap never reached"
      | Some n ->
          match n.rk with
          | 0 -> a + 1
          | _ -> a + 1 + (num_nodes n.c) ) 0 h

(*
  (* Finds number of nodes inside a Fibonacci heap *)
  let rec num_nodes (h: heap) : int =
    lnk_lst_fold (fun a h' ->
      (* Printf.printf "acc value = %i \n" a; *)
      match !h' with
      | None -> 0
      | Some n -> 
	a + 1 + (
	  (List.iter 
	     ~f:(fun h' -> 
	       (if phys_equal h' h
		then Printf.printf "LOOP EXISTS \n"
		else test_list := h::!test_list
	       )
	     ) 
	     !test_list
	  );
	  num_nodes n.c)) 0 h
*)

  (* Inserts a list of pairs into the given heap and returns a handle to the
   * resulting heap as well as a list of nodes corresponding to each pair,
   * in the same order as the original pair list it corresponds to. *)
  let insert_list (h: heap) (lst: (key * value) list) : heap * heap list =
    let insert_keep_track (k,v) r =
      let (sofar,hs) = r in
      let size1 = num_nodes sofar in
      (* Printf.printf "num_nodes works on heap of size %i \n" size1; *)
      let (whole,mine) = insert k v sofar in 
      (* Printf.printf "insert function just finished \n"; *)
      (*  if !whole = None 
	  then Printf.printf "None child \n"
	  else Printf.printf "Some child \n"); *)
      assert(size1 + 1 = num_nodes whole) ; 
      (* Printf.printf "assert on size %i \n" size1; *)
      (whole, mine::hs)
    in
    List.fold_right lst ~init:(h,[]) ~f:insert_keep_track 

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

  let top_matches ((ka,_): (key * value)) (pt: heap) : bool =
    match get_top_node pt with
    | None -> false
    | Some (kb,_) ->
        (match H.compare ka kb with
        | Equal -> true
        | _ -> false)
    
  let test_insert () = 
    (* Insert a single pair *)
    let (k,v) = H.gen_pair () in
    let (hp,nd) = insert k v empty in
    assert(top_matches (k,v) nd);
    assert(top_matches (k,v) hp);
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
    assert((num_nodes h1) = 100) ;
    (* Rinse and repeat with a reverse-sequential list *)
    let revpairs = List.rev seqpairs in
    let (h3,lst3) = insert_list empty revpairs in
    List.iter2_exn ~f:(fun a pt -> assert(top_matches a pt)) revpairs lst3 ;
    assert((List.hd seqpairs) = (get_top_node h3)) ;
    assert((num_nodes h1) = 100) ;
    ()

  let test_decrease_key () =
    (* Fill heap with identical pairs *)
    let key1 = H.gen_key() in
    let identpairs = generate_identical_list key1 100 in
    let (idheap, idlist) = insert_list empty identpairs in
    let (id1,id2,id3) = match idlist with
      | [] -> failwith "list can't be empty"
      | id1::id2::id3::_ -> id1,id2,id3
      | _ -> failwith "list must have 100 nodes" in
    let key0 = H.gen_key_lt (H.gen_key_lt key1 ()) () in
    let heap1 = decrease_key id1 key0 idheap in
    match get_top_node heap1 with
    | None -> failwith "not possible 0"
    | Some (k, _) -> assert(H.compare k key0 = Equal);
    let keymid = match H.gen_key_between key0 key1 () with
      | None -> failwith "not possible 1"
      | Some keymid -> keymid in
    let heap2 = decrease_key id2 keymid heap1 in
    let key2 = match get_top_node heap2 with
      | None -> failwith "heap cannot be empty"
      | Some (key2,_) -> key2 in
    assert(key0 = key2) ;
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
		    (*assert(!(n.p) = None) ;*) nh) ~init:seqheap' seqlst'
    in
    assert((let n = lnk_lst_fold (fun a _ -> a+1) 0 seqheap'' in 
    Printf.printf " %i \n" n; n) = (num_nodes seqheap')) ;
    assert((num_nodes seqheap'') = (num_nodes seqheap')) ;
    ()
    
  let test_delete_min () =
    let onepair = generate_pair_list 1 in
    let (k,v) = match onepair with
      | [] -> failwith "list is not empty"
      | (a,b)::_ -> (a,b)
    in
    let (oneheap, onelst) = insert_list empty [(k,v)] in
    assert(not (is_empty oneheap)) ;
    assert(not ((List.hd onelst) = None)) ;
    (* Printf.printf "starting oneheap test \n"; *)
    let (oneheap, _) = insert k v empty in
    (* Printf.printf "oneheap and onelst created \n"; *)
    let (k1,v1),emptyheap = match delete_min oneheap with
      | None,_ -> failwith "heap is not empty"
      | (Some kv),h -> kv,h
    in
    assert(num_nodes oneheap = 1) ;
    assert((k1,v1) = (k,v)) ;
    assert( is_empty emptyheap) ;
    (*let seqpairs = generate_pair_list 100 in
    let (seqheap, seqlst) = insert_list empty seqpairs in
    let emptyheap = List.fold_left ~f:(fun h t ->
      let beforesize = num_nodes t in
      let (kv_op, nh) = delete_min t in
      let (k,v) = match kv_op with
        | None -> failwith "all nodes are real"
        | Some (k,v) -> k,v
      in
      assert(Some (k,v) = get_top_node h) ;
      let aftersize = num_nodes nh in
      assert(beforesize = aftersize + 1) ; nh) ~init:seqheap seqlst in 
    assert(is_empty emptyheap) ;*)
    ()

  let run_tests () =
(*    test_insert () ;
    test_decrease_key () ;
    test_delete_min () ;*)
    ()

end

(* Create a fib heap with (int, string) pairs for testing *)
module IntStringFibHeap : (PRIOHEAP with type value = IntStringHeapArg.value
    with type key = IntStringHeapArg.key) = FibonacciHeap(IntStringHeapArg) ;;
(* Uncomment the following when ready to run tests on fib heap *)
IntStringFibHeap.run_tests()

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
        match (Distance.distance cutoff loc1 loc2) with
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
  let cutoff = (try (Float.of_string (Sys.argv.(2))) with
                | _ -> Printf.printf 
                  "'%s' is an invalid cutoff value\n
                  cutoff value must be numerical\n"
                  Sys.argv.(2); exit 1)
  in
  let csv = (match Sys.file_exists (Sys.argv.(1)) with
             | `Yes -> Sys.argv.(1)
             | _ -> Printf.printf 
                  "file '%s' not found\n"
                  Sys.argv.(1); exit 1) in 
  let delimiter = Regex.create_exn ";" in
  let parse_line line = Regex.split delimiter line in
  let rec cast (parseline: string list) : (string * (float * float)) option =
     match parseline with
     | [name;lat;lng] ->
         Some (name,((Float.of_string lat),(Float.of_string lng)))
     | _ -> None
  in
  let file = In_channel.create csv in
  let get_lines f : string list =
    let rec lines_helper f lst =
      match In_channel.input_line ~fix_win_eol:true f with
      | None -> lst
      | Some l -> lines_helper f (l::lst)
    in
    lines_helper f []
  in
  let lines = get_lines file in
  let parse_and_cast line = cast (parse_line line) in
  let casted =
    List.fold_left lines ~f:(fun lst ln -> match parse_and_cast ln with
                            | Some p -> p::lst
                            | None -> lst)
                         ~init:[]
  in
  let _ = In_channel.close file in
  (* Read in and parse the file into a string list list. *)
  (*let casted = In_channel.with_file csv
               ~f:(fun file -> In_channel.fold_lines file ~init:[]
               ~f:(fun lst ln -> match parse_and_cast ln with
                                 | Some p -> p::lst
                                 | None -> lst))
  in*)
  addedges casted GeoGraph.empty cutoff






(* Request start and finish nodes from user *)
let rec get_nodes (g: GeoGraph.graph) : GeoNode.node * GeoNode.node =
  (* get_nodes should actually return an option GeoNode.node * GeoNode.node *) 
  (* Should give the user a text prompt so they know what to input *)
  let () = Printf.printf "Origin City: " in
  let st = read_line () in
  let try_again () = Printf.printf ("City not in database. \n
  Please make sure that you type in the city_name comma state_abbreviation \n
  For example: New York City, NY\n") in
  let stnode = GeoNode.node_of_tag st in
  if (not (GeoGraph.has_node g stnode)) then
    let _ = try_again () in get_nodes g
  else 
    let () = Printf.printf "Destination City: " in
    let fin = read_line () in
    let finnode = GeoNode.node_of_tag fin in
    if (not (GeoGraph.has_node g finnode)) then
      let _ = try_again () in get_nodes g
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
  | hd::tl -> Printf.printf "%s -> " (GeoNode.tag_of_node hd) ; printnodes tl
in
printnodes nodelist; Printf.printf "\nTotal distance: %f km\n" weight; ()


