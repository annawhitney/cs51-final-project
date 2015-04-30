open Core.Std

(* Interfaces and implementations of dictionaries.  A dictionary
 * is used to associate a value with a key. Dictionary interface and
 * implementation borrowed from Moogle.
 *)
exception TODO

module type DICT =
sig
  type key
  type value
  type dict

  (* An empty dictionary *)
  val empty : dict

  (* Reduce the dictionary using the provided function f and base case u.
   * Our reducing function f must have the type:
   *      key -> value -> 'a -> 'a
   * and our base case u has type 'a.
   *
   * If our dictionary is the (key,value) pairs (in any order)
   *      (k1,v1), (k2,v2), (k3,v3), ... (kn,vn)
   * then fold should return:
   *      f k1 v1 (f k2 v2 (f k3 v3 (f ... (f kn vn u))))
   *)
  val fold : (key -> value -> 'a -> 'a) -> 'a -> dict -> 'a

  (* Returns as an option the value associated with the provided key. If
   * the key is nodeot in the dictionary, return None. *)
  val lookup : dict -> key -> value option

  (* Returns true if and only if the key is in the dictionary. *)
  val member : dict -> key -> bool

  (* Returns the key if the key is in the dictionary. Useful for finding what
   * the actual key is when two distinct keys can compare equal. *)
  val verify_key : dict -> key -> key option

  (* Inserts a (key,value) pair into our dictionary. If the key is already
   * in our dictionary, update the key to have the new value. *)
  val insert : dict -> key -> value -> dict

  (* Removes the given key from the dictionary. If the key is not present,
   * return the original dictionary. *)
  val remove : dict -> key -> dict

  (* Return an arbitrary key, value pair along with a new dict with that
   * pair removed. Return None if the input dict is empty *)
  val choose : dict -> (key * value * dict) option

  (* functions to convert our types to strings for debugging and logging *)
  val string_of_key: key -> string
  val string_of_value : value -> string
  val string_of_dict : dict -> string

  (* Runs all the tests. see TESTING EXPLANATION below *)
  val run_tests : unit -> unit
end



(* Argument module signature to our DICT functors *)
module type DICT_ARG =
sig
  type key
  type value
  val compare : key -> key -> Ordering.t
  val string_of_key : key -> string
  val string_of_value : value -> string

  (* Use these functions for testing. See TESTING EXPLANATION. *)

  (* Generate a key. The same key is always returned *)
  val gen_key : unit -> key

  (* Generate a random key. *)
  val gen_key_random : unit -> key

  (* Generates a key greater than the argument. *)
  val gen_key_gt : key -> unit -> key

  (* Generates a key less than the argument. *)
  val gen_key_lt : key -> unit -> key

  (* Generates a key between the two arguments. Return None if no such
   * key exists. *)
  val gen_key_between : key -> key -> unit -> key option

  (* Generates a random value. *)
  val gen_value : unit -> value

  (* Generates a random (key,value) pair *)
  val gen_pair : unit -> key * value
end



(* An example implementation of our DICT_ARG signature. Use this struct
 * for testing. *)
module IntStringDictArg : DICT_ARG =
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


(******************************************************************)
(* BTDict: a functor that implements our DICT signature           *)
(* using a balanced tree (2-3 trees)                              *)
(******************************************************************)
module BTDict(D:DICT_ARG) : (DICT with type key = D.key
with type value = D.value) =
struct
  open Order

  type key = D.key
  type value = D.value

  (* A dictionary entry is a (key,value) pair. We compare two (key,value)
   * pairs with the provided key-comparison function D.compare. For example,
   * we may choose to keep a dictionary mapping links to their ranks. In this
   * case, our (key,value) pairs will be (link,rank) pairs, and we compare
   * links using string comparison. *)
  type pair = key * value

  (* Type definition for dictionary, which we choose to represent as a 2-3 Tree.
   * This is almost the same as the binary search tree definition from pset4 and
   * lecture, except we add one more case: a Three-node.
   *
   * A Three-node contains two pairs and three subtrees: left, middle, and
   * right, represented by the 3 dicts in the definition below. *)
  type dict =
    | Leaf
    | Two of dict * pair * dict
    | Three of dict * pair * dict * pair * dict

  (* INVARIANTS:
   * 2-node: Two(left,(k1,v1),right)
   * (1) Every key k appearing in subtree left must be k < k1.
   * (2) Every key k appearing in subtree right must be k > k1.
   * (3) The length of the path from the 2-node to
   *     every leaf in its two subtrees must be the same.
   *
   * 3-node: Three(left,(k1,v1),middle,(k2,v2),right)
   * (1) k1 < k2.
   * (2) Every key k appearing in subtree left must be k < k1.
   * (3) Every key k appearing in subtree right must be k > k2.
   * (4) Every key k appearing in subtree middle must be k1 < k < k2.
   * (5) The length of the path from the 3-node to every leaf in its three
   *     subtrees must be the same.
   *)

  (* FOR INSERTION:
   * A kicked configuration returned by going downwards on insertion.
   * We can only kick up Two nodes, hence Up takes a dict * pair * dict *)
  type kicked =
    | Up of dict * pair * dict
    | Done of dict

  (* FOR REMOVAL:
   * A hole configuration returned by going downwards on removal. We
   * include a pair option whenever we remove the minimum of the right
   * subtree of the current pair in order the current pair *)
  type hole =
    | Hole of pair option * dict
    | Absorbed of pair option * dict

  (* FOR REMOVAL:
   * A direction will distinguish which configuration we came from in the
   * removal cases. We use direction2 for cases (1-2) on the handout, and
   * we use direction3 for cases (3-4) on the handout. *)
  type direction2 =
    | Left2
    | Right2

  type direction3 =
    | Left3
    | Mid3
    | Right3

  (* How do we represent an empty dictionary with 2-3 trees? *)
  let empty : dict = Leaf

  (* Implement fold. Read the specification in the DICT signature above. *)
  let rec fold (f: key -> value -> 'a -> 'a) (u: 'a) (d: dict) : 'a =
    match d with
    | Leaf -> u
    | Two (d1,(k,v),d2) -> fold f (fold f (f k v u) d1) d2
    | Three (d1,(k1,v1),d2,(k2,v2),d3) ->
        fold f (fold f (f k2 v2 (fold f (f k1 v1 u) d1)) d2) d3

  (* Implement these to-string functions *)
  let string_of_key = D.string_of_key
  let string_of_value = D.string_of_value
  let string_of_dict (d: dict) : string = 
    let f = (fun k v u -> u ^ "\n key: " ^ D.string_of_key k ^
      "; value: (" ^ D.string_of_value v ^ ")") in
    fold f "" d

  (* Debugging function. This will print out the tree in text format.
   * Use this function to see the actual structure of your 2-3 tree. *
   *
   * e.g.      (4,d)   (6,f)
   *         /       |       \
   *      (2,b)    (4,d)     Leaf
   *      /  \     /   \
   *   Leaf  Leaf Leaf  Leaf
   *
   * string_of_tree will output:
   * Three(Two(Leaf,(2,b),Leaf),(4,d),Two(Leaf,(5,e),Leaf),(6,f),Leaf)
   *
   * Note that this tree is NOT balanced, because all the paths from (6,f)
   * to its leaves do NOT all have the same length. *)
  (*
  let rec string_of_tree (d: dict) : string =
    match d with
      | Leaf -> "Leaf"
      | Two(left,(k,v),right) -> "Two(" ^ (string_of_tree left)
        ^ ",(" ^ (string_of_key k) ^ "," ^ (string_of_value v) ^ "),"
        ^ (string_of_tree right) ^ ")"
      | Three(left,(k1,v1),middle,(k2,v2),right) ->
        "Three(" ^ (string_of_tree left)
        ^ ",(" ^ (string_of_key k1) ^ "," ^ (string_of_value v1) ^ "),"
        ^ (string_of_tree middle) ^ ",(" ^ (string_of_key k2) ^ ","
        ^ (string_of_value v2) ^ ")," ^ (string_of_tree right) ^ ")"
  *)

  (* Upward phase for w where its parent is a Two node whose (key,value) is x.
   * One of x's children is w, and the other child is x_other. This function
   * should return a kicked-up configuration containing the new tree as a
   * result of performing the upward phase on w. *)
  let insert_upward_two (w: pair) (w_left: dict) (w_right: dict)
      (x: pair) (x_other: dict) : kicked =
    let (kw,_) = w in
    let (kx,_) = x in
    match D.compare kw kx with
    | Equal -> failwith "Equal keys should be handled going downward"
    | Less -> Done (Three(w_left,w,w_right,x,x_other))
    | Greater -> Done (Three(x_other,x,w_left,w,w_right))

  (* Upward phase for w where its parent is a Three node whose (key,value) is x.
   * One of x's children is w, and of the two remaining children,
   * other_left is the subtree more to the left and other_right is the
   * subtree more to the right.
   *
   * E.g. From our handout, for the first case where w's parent is a Three-tree,
   * other_left would be c and other_right would be d. For the second case,
   * other_left would be a and other_right would be d. For the third case,
   * other_left would be a and other_right would be b.
   *
   * This function should return a kicked-up configuration containing the
   * new tree as a result of performing the upward phase on w. *)
  let insert_upward_three (w: pair) (w_left: dict) (w_right: dict)
      (x: pair) (y: pair) (other_left: dict) (other_right: dict) : kicked =
    let (kw,_) = w in
    let (kx,_) = x in
    let (ky,_) = y in
    match D.compare kw kx with
    | Equal -> failwith "Equal keys should be handled going downward"
    | Less -> Up (Two(w_left,w,w_right),x,Two(other_left,y,other_right))
    | Greater -> 
        match D.compare kw ky with
        | Equal -> failwith "Equal keys should be handled going downward"
        | Less -> Up (Two(other_left,x,w_left),w,Two(w_right,y,other_right))
        | Greater -> Up (Two(other_left,x,other_right),y,Two(w_left,w,w_right))

  (* Downward phase for inserting (k,v) into our dictionary d.
   * The downward phase returns a "kicked" up configuration, where
   *
   * type kicked =
   *      | Up of dict * pair * dict
   *      | Done of dict
   *
   * A kicked up configuration can only be a Two node, hence the Up
   * constructor takes the same parameters as the Two constructor. We return
   * Up(left,(k,v),right) if the Two-node represented by this Up needs to
   * be further kicked up in the upward phase (this is represented by an up
   * arrow on the 2-3 Tree handout). We return Done(d) if we have finished
   * our upward phase on the tree represented by d.
   *
   * The functions insert_downward, insert_downward_two, and
   * insert_downward_three are __mutually recursive__, hence the
   * "let rec" and the "and" keywords. Here, we use three mutually recursive
   * functions to simplify our code into smaller pieces.
   *
   * Two functions f and g are __mutually recursive__ if in f's definition,
   * f calls g, and in g's definition, g calls f. This definition of
   * mutually recursive definitions can be extended to more than two functions,
   * as follows:
   *
   * Functions f1, f2, f3, ..., fn are mutually recursive if for each of
   * these functions f, all of the other f_i's can be called on some execution
   * of f. *)

  (* insert_downward should handle the base case when inserting into a Leaf,
   * and if our dictionary d is a Two-node or a Three-node, we call the
   * corresponding functions insert_downward_two or insert_downward_three
   * with the appropriate arguments. *)
  let rec insert_downward (d: dict) (k: key) (v: value) : kicked =
    match d with
      | Leaf -> Up(Leaf,(k,v),Leaf)
      | Two(left,n,right) -> insert_downward_two (k,v) n left right
      | Three(left,n1,middle,n2,right) ->
          insert_downward_three (k,v) n1 n2 left middle right

  (* Downward phase on a Two node. (k,v) is the (key,value) we are inserting,
   * (k1,v1) is the (key,value) of the current Two node, and left and right
   * are the two subtrees of the current Two node. *)
  and insert_downward_two ((k,v): pair) ((k1,v1): pair)
      (left: dict) (right: dict) : kicked =
    match D.compare k k1 with
    | Equal -> Done (Two(left,(k1,v),right))
    | Less -> 
        (match insert_downward left k v with
        | Up(l,p,r) -> insert_upward_two p l r (k1,v1) right
        | Done d -> Done (Two(d,(k1,v1),right)))
    | Greater -> 
        (match insert_downward right k v with
        | Up(l,p,r) -> insert_upward_two p l r (k1,v1) left
        | Done d -> Done (Two(left,(k1,v1),d)))

  (* Downward phase on a Three node. (k,v) is the (key,value) we are inserting,
   * (k1,v1) and (k2,v2) are the two (key,value) pairs in our Three node, and
   * left, middle, and right are the three subtrees of our current Three node *)
  and insert_downward_three ((k,v): pair) ((k1,v1): pair) ((k2,v2): pair)
      (left: dict) (middle: dict) (right: dict) : kicked =
    match D.compare k k1 with
    | Equal -> Done (Three(left,(k1,v),middle,(k2,v2),right))
    | Less ->
        (match insert_downward left k v with
        | Up(l,p,r) -> insert_upward_three p l r (k1,v1) (k2,v2) middle right
        | Done d -> Done (Three(d,(k1,v1),middle,(k2,v2),right)))
    | Greater ->
        (match D.compare k k2 with
        | Equal -> Done (Three(left,(k1,v1),middle,(k2,v),right))
        | Less ->
            (match insert_downward middle k v with
            | Up(l,p,r) -> insert_upward_three p l r (k1,v1) (k2,v2) left right
            | Done d -> Done (Three(left,(k1,v1),d,(k2,v2),right)))
        | Greater ->
            (match insert_downward right k v with
            | Up(l,p,r) ->
                insert_upward_three p l r (k1,v1) (k2,v2) left middle
            | Done d -> Done (Three(left,(k1,v1),middle,(k2,v2),d))))

  (* We insert (k,v) into our dict using insert_downward, which gives us
   * "kicked" up configuration. We return the tree contained in the "kicked"
   * configuration. *)
  let insert (d: dict) (k: key) (v: value) : dict =
    match insert_downward d k v with
      | Up(l,(k1,v1),r) -> Two(l,(k1,v1),r)
      | Done x -> x

  (* Upward phase for removal where the parent of the hole is a Two node.
   * See cases (1-2) on the handout. n is the (key,value) pair contained in
   * the parent node; left and right are the subtrees of the parent node (our
   * hole is one of these subtrees); and dir indicates which subtree was
   * contained by the hole. *)
  let remove_upward_two (n: pair) (rem: pair option)
      (left: dict) (right: dict) (dir: direction2) : hole =
    match dir,n,left,right with
      | Left2,x,l,Two(m,y,r) -> Hole(rem,Three(l,x,m,y,r))
      | Right2,y,Two(l,x,m),r -> Hole(rem,Three(l,x,m,y,r))
      | Left2,x,a,Three(b,y,c,z,d) ->
   Absorbed(rem,Two(Two(a,x,b),y,Two(c,z,d)))
      | Right2,z,Three(a,x,b,y,c),d ->
   Absorbed(rem,Two(Two(a,x,b),y,Two(c,z,d)))
      | Left2,_,_,_ | Right2,_,_,_ -> Absorbed(rem,Two(Leaf,n,Leaf))

  (* Upward phase for removal where the parent of the hole is a Three node.
   * See cases (3-4) on the handout. n1 and n2 are the (key,value) pairs
   * contained in the parent node; left, middle, and right are the subtrees
   * of the parent node (our hole is one of these subtrees); and dir indicates
   * which subtree was the tree contained by the hole. *)
  let remove_upward_three (n1: pair) (n2: pair) (rem: pair option)
      (left: dict) (middle: dict) (right: dict) (dir: direction3) : hole =
    match dir,n1,n2,left,middle,right with
      | Left3,x,z,a,Two(b,y,c),d -> Absorbed(rem,Two(Three(a,x,b,y,c),z,d))
      | Mid3,y,z,Two(a,x,b),c,d -> Absorbed(rem,Two(Three(a,x,b,y,c),z,d))
      | Mid3,x,y,a,b,Two(c,z,d) -> Absorbed(rem,Two(a,x,Three(b,y,c,z,d)))
      | Right3,x,z,a,Two(b,y,c),d -> Absorbed(rem,Two(a,x,Three(b,y,c,z,d)))
      | Left3,w,z,a,Three(b,x,c,y,d),e ->
   Absorbed(rem,Three(Two(a,w,b),x,Two(c,y,d),z,e))
      | Mid3,y,z,Three(a,w,b,x,c),d,e ->
   Absorbed(rem,Three(Two(a,w,b),x,Two(c,y,d),z,e))
      | Mid3,w,x,a,b,Three(c,y,d,z,e) ->
   Absorbed(rem,Three(a,w,Two(b,x,c),y,Two(d,z,e)))
      | Right3,w,z,a,Three(b,x,c,y,d),e ->
   Absorbed(rem,Three(a,w,Two(b,x,c),y,Two(d,z,e)))
      | Left3,_,_,_,_,_ | Mid3,_,_,_,_,_ | Right3,_,_,_,_,_ ->
        Absorbed(rem,Three(Leaf,n1,Leaf,n2,Leaf))

  (* DO NOT EDIT THIS *)
  let rec remove_downward (d: dict) (k: key) : hole =
    match d with
      | Leaf -> Absorbed(None,d)
      | Two(Leaf,(k1,v1),Leaf) ->
        (match D.compare k k1 with
          | Equal -> Hole(Some(k1,v1),Leaf)
          | Less | Greater -> Absorbed(None,d)
        )
      | Three(Leaf,(k1,v1),Leaf,(k2,v2),Leaf) ->
        (match D.compare k k1, D.compare k k2 with
          | Equal, _ -> Absorbed(Some(k1,v1),Two(Leaf,(k2,v2),Leaf))
          | _, Equal -> Absorbed(Some(k2,v2),Two(Leaf,(k1,v1),Leaf))
          | _, _ -> Absorbed(None,d)
        )
      | Two(l,n,r) -> remove_downward_two k n l r
      | Three(l,n1,m,n2,r) -> remove_downward_three k n1 n2 l m r

  (* DO NOT EDIT THIS *)
  and remove_downward_two (k: key) ((k1,v1): pair)
      (left: dict) (right: dict) : hole =
    match D.compare k k1 with
      | Equal ->
        (match remove_min right with
          | Hole(None,_) -> Hole(None,left)
          | Hole(Some n,new_right) ->
            remove_upward_two n None left new_right Right2
          | Absorbed(None,_) -> Hole(None,left)
          | Absorbed(Some n,new_right) -> Absorbed(None,Two(left,n,new_right))
        )
      | Less ->
        (match remove_downward left k with
          | Hole(rem,t) -> remove_upward_two (k1,v1) rem t right Left2
          | Absorbed(rem,t) -> Absorbed(rem,Two(t,(k1,v1),right))
        )
      | Greater ->
        (match remove_downward right k with
          | Hole(rem,t) -> remove_upward_two (k1,v1) rem left t Right2
          | Absorbed(rem,t) -> Absorbed(rem,Two(left,(k1,v1),t))
        )

  (* DO NOT EDIT THIS *)
  and remove_downward_three (k: key) ((k1,v1): pair) ((k2,v2): pair)
      (left: dict) (middle: dict) (right: dict) : hole =
    match D.compare k k1, D.compare k k2 with
      | Equal, _ ->
        (match remove_min middle with
          | Hole(None,_) -> Hole(None,Two(left,(k2,v2),right))
          | Hole(Some n,new_middle) ->
            remove_upward_three n (k2,v2) None left new_middle right Mid3
          | Absorbed(None,_) -> Absorbed(None,Two(left,(k1,v1),right))
          | Absorbed(Some n,new_middle) ->
            Absorbed(None,Three(left,n,new_middle,(k2,v2),right))
        )
      | _ , Equal ->
        (match remove_min right with
          | Hole(None,_) -> Hole(None,Two(left,(k1,v1),middle))
          | Hole(Some n,new_right) ->
            remove_upward_three (k1,v1) n None left middle new_right Right3
          | Absorbed(None,_) -> Absorbed(None,Two(left,(k1,v1),middle))
          | Absorbed(Some n,new_right) ->
            Absorbed(None,Three(left,(k1,v1),middle,n,new_right))
        )
      | Less, _ ->
        (match remove_downward left k with
          | Hole(rem,t) ->
            remove_upward_three (k1,v1) (k2,v2) rem t middle right Left3
          | Absorbed(rem,t) ->
            Absorbed(rem,Three(t,(k1,v1),middle,(k2,v2),right))
        )
      | _, Greater ->
        (match remove_downward right k with
          | Hole(rem,t) ->
            remove_upward_three (k1,v1) (k2,v2) rem left middle t Right3
          | Absorbed(rem,t) ->
            Absorbed(rem,Three(left,(k1,v1),middle,(k2,v2),t))
        )
      | Greater, Less ->
        (match remove_downward middle k with
          | Hole(rem,t) ->
            remove_upward_three (k1,v1) (k2,v2) rem left t right Mid3
          | Absorbed(rem,t) ->
            Absorbed(rem,Three(left,(k1,v1),t,(k2,v2),right))
        )

  (* DO NOT EDIT THIS *)
  and remove_min (d: dict) : hole =
    match d with
      | Leaf -> Hole(None,Leaf)
      | Two(Leaf,n,_) -> Hole(Some n,Leaf)
      | Three(Leaf,n1,middle,n2,right) -> Absorbed(Some n1,Two(middle,n2,right))
      | Two(left,n,right) ->
        (match remove_min left with
          | Hole(rem,t) -> remove_upward_two n rem t right Left2
          | Absorbed(rem,t) -> Absorbed(rem,Two(t,n,right))
        )
      | Three(left,n1,middle,n2,right) ->
        (match remove_min left with
          | Hole(rem,t) -> remove_upward_three n1 n2 rem t middle right Left3
          | Absorbed(rem,t) -> Absorbed(rem,Three(t,n1,middle,n2,right))
        )

  (* DO NOT EDIT THIS *)
  let remove (d: dict) (k: key) : dict =
    match remove_downward d k with
      | Hole(_,d') -> d'
      | Absorbed(_,d') -> d'

  (* Write a lookup function that returns the value of the given key
   * in our dictionary and returns it as an option, or return None
   * if the key is not in our dictionary. *)
  let rec lookup (d: dict) (k: key) : value option =
    match d with
    | Leaf -> None
    | Two(l,(k1,v1),r) ->
        (match D.compare k k1 with
        | Equal -> Some v1
        | Less -> lookup l k
        | Greater -> lookup r k)
    | Three(l,(k1,v1),m,(k2,v2),r) ->
        (match D.compare k k1 with
        | Equal -> Some v1
        | Less -> lookup l k
        | Greater ->
            (match D.compare k k2 with
            | Equal -> Some v2
            | Less -> lookup m k
            | Greater -> lookup r k))

  (* Write a function to test if a given key is in our dictionary *)
  let rec member (d: dict) (k: key) : bool =
    match d with
    | Leaf -> false
    | Two(l,(k1,_),r) ->
        (match D.compare k k1 with
        | Equal -> true
        | Less -> member l k
        | Greater -> member r k)
    | Three(l,(k1,_),m,(k2,_),r) ->
        (match D.compare k k1 with
        | Equal -> true
        | Less -> member l k
        | Greater ->
            (match D.compare k k2 with
            | Equal -> true
            | Less -> member m k
            | Greater -> member r k))

  (* A function to return the key (if present) that compares equal to a given
   * key; useful when the comparison functions ignores part of a key. *)
  let rec verify_key (d: dict) (k: key) : key option =
    match d with
    | Leaf -> None
    | Two(l,(k1,_),r) ->
        (match D.compare k k1 with
        | Equal -> Some k1
        | Less -> verify_key l k
        | Greater -> verify_key r k)
    | Three(l,(k1,_),m,(k2,_),r) ->
        (match D.compare k k1 with
        | Equal -> Some k1
        | Less -> verify_key l k
        | Greater ->
            (match D.compare k k2 with
            | Equal -> Some k2
            | Less -> verify_key m k
            | Greater -> verify_key r k))


  (* Write a function that removes any (key,value) pair from our
   * dictionary (your choice on which one to remove), and returns
   * as an option this (key,value) pair along with the new dictionary.
   * If our dictionary is empty, this should return None. *)
  let choose (d: dict) : (key * value * dict) option =
    match d with
    | Leaf -> None
    | Two(_,(k,v),_) | Three(_,(k,v),_,_,_) -> Some (k,v,remove d k)

  (* Write a function that when given a 2-3 tree (represented by our
   * dictionary d), returns true if and only if the tree is "balanced",
   * where balanced means that the given tree satisfies the 2-3 tree
   * invariants stated above and in the 2-3 tree handout. *)

  (* How are you testing that you tree is balanced?
   * ANSWER:
   *    By checking that its subtrees are balanced and have the same height.
   *)
  let balanced (d: dict) : bool =
    let rec balanced_helper d : bool * int =
      match d with
      | Leaf -> (true, 0)
      | Two (d1,_,d2) ->
          let (b1,h1) = balanced_helper d1 in
          let (b2,h2) = balanced_helper d2 in
          (b1 && b2 && (h1 = h2), (h1 + 1))
      | Three (d1,_,d2,_,d3) ->
          let (b1,h1) = balanced_helper d1 in
          let (b2,h2) = balanced_helper d2 in
          let (b3,h3) = balanced_helper d3 in
          (b1 && b2 && b3 && (h1 = h2) && (h1 = h3), (h1 + 1))
    in
    let (b,_) = balanced_helper d in b


  (********************************************************************)
  (*       TESTS                                                      *)
  (* You must write more comprehensive tests, using our remove tests  *)
  (* below as an example                                              *)
  (********************************************************************)

  (* adds a list of (key,value) pairs in left-to-right order *)
  let insert_list (d: dict) (lst: (key * value) list) : dict =
    List.fold_left lst ~f:(fun r (k,v) -> insert r k v) ~init:d

  (* adds a list of (key,value) pairs in right-to-left order *)
  let insert_list_reversed (d: dict) (lst: (key * value) list) : dict =
    List.fold_right lst ~f:(fun (k,v) r -> insert r k v) ~init:d

  (* generates a (key,value) list with n distinct keys in increasing order,
   * starting from a given key *)
  let rec gen_pairs_from (size: int) (current: key) : (key * value) list =
    if size <= 0 then []
    else
      let new_current = D.gen_key_gt current () in
      (new_current, D.gen_value()) :: (gen_pairs_from (size - 1) new_current)

  (* generates a (key,value) list with n distinct keys in increasing order *)
  let generate_pair_list (size: int) : (key * value) list =
    gen_pairs_from size (D.gen_key ())

  (* generates a (key,value) list with keys in random order *)
  let rec generate_random_list (size: int) : (key * value) list =
    if size <= 0 then []
    else
      (D.gen_key_random(), D.gen_value()) :: (generate_random_list (size - 1))

  (* generates a (key,value) list with identical keys *)
  let rec generate_identical_list (size: int) : (key * value) list =
    if size <= 0 then []
    else
      (D.gen_key(), D.gen_value()) :: (generate_identical_list (size - 1))

  let rec dict_size (d: dict) =
    match choose d with
    | None -> 0
    | Some (_,_,d1) -> 1 + (dict_size d1)

  let test_balance () =
    let d1 = Leaf in
    assert(balanced d1) ;

    let d2 = Two(Leaf,D.gen_pair(),Leaf) in
    assert(balanced d2) ;

    let d3 = Three(Leaf,D.gen_pair(),Leaf,D.gen_pair(),Leaf) in
    assert(balanced d3) ;

    let d4 = Three(Two(Two(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                           Two(Leaf,D.gen_pair(),Leaf)),
                       D.gen_pair(),Two(Two(Leaf,D.gen_pair(),Leaf),
                                        D.gen_pair(),
                                        Two(Leaf,D.gen_pair(),Leaf))),
                   D.gen_pair(),
                   Two(Two(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                           Two(Leaf,D.gen_pair(),Leaf)),D.gen_pair(),
                       Two(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                           Two(Leaf,D.gen_pair(),Leaf))),D.gen_pair(),
                   Two(Two(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                           Two(Leaf,D.gen_pair(),Leaf)),D.gen_pair(),
                       Three(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                             Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                             Three(Leaf,D.gen_pair(),Leaf,D.gen_pair(),Leaf))))
    in
    assert(balanced d4) ;

    let d5 = Two(Leaf,D.gen_pair(),Two(Leaf,D.gen_pair(),Leaf)) in
    assert(not (balanced d5)) ;

    let d6 = Three(Leaf,D.gen_pair(),
                   Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),Leaf) in
    assert(not (balanced d6)) ;

    let d7 = Three(Three(Leaf,D.gen_pair(),Leaf,D.gen_pair(),Leaf),
                   D.gen_pair(),Leaf,D.gen_pair(),Two(Leaf,D.gen_pair(),Leaf))
    in
    assert(not (balanced d7)) ;
    () 

  (* Insert a random list of pairs and test they're all in there. *)
  let test_insert_random () =
    let pairs = generate_random_list 26 in
    let d1 = insert_list empty pairs in
    List.iter pairs ~f:(fun (k,_) -> assert(member d1 k)) ;
    assert(balanced d1) ;
    ()

  (* Insert a sequential list of pairs and test they're all in there. *)
  let test_insert_sequential () =
    let pairs = generate_pair_list 26 in
    let d1 = insert_list empty pairs in
    List.iter pairs ~f:(fun (k,v) -> assert(lookup d1 k = Some v)) ;
    assert(dict_size d1 = 26) ;
    assert(balanced d1) ;
    ()

  (* Insert a sequential list of pairs in reverse order
   * and test they're all in there. *)
  let test_insert_reversed () =
    let pairs = generate_pair_list 26 in
    let d1 = insert_list_reversed empty pairs in
    List.iter pairs ~f:(fun (k,v) -> assert(lookup d1 k = Some v)) ;
    assert(dict_size d1 = 26) ;
    assert(balanced d1) ;
    ()

  (* Insert a list of pairs with identical keys and different values
   * and test that the final value is correct. *)
  let test_insert_identical () =
    let pairs = generate_identical_list 26 in
    let d1 = insert_list empty pairs in
    (* Value associated with the key should be the last one in the list *)
    match List.rev pairs with
    | [] -> failwith "list of pairs should not be empty"
    | (k,v1)::_ ->
      assert(lookup d1 k = Some v1) ;
      assert(dict_size d1 = 1) ;
      assert(balanced d1) ;
    ()

  let test_remove_nothing () =
    let pairs1 = generate_pair_list 26 in
    let d1 = insert_list empty pairs1 in
    let r2 = remove d1 (D.gen_key_lt (D.gen_key()) ()) in
    List.iter pairs1 ~f:(fun (k,v) -> assert(lookup r2 k = Some v)) ;
    assert(balanced r2) ;
    ()

  let test_remove_from_nothing () =
    let d1 = empty in
    let r1 = remove d1 (D.gen_key()) in
    assert(r1 = empty) ;
    assert(balanced r1) ;
    ()

  let test_remove_in_order () =
    let pairs1 = generate_pair_list 26 in
    let d1 = insert_list empty pairs1 in
    List.iter
      pairs1
      ~f:(fun (k,_) ->
        let r = remove d1 k in
        let _ = List.iter
          pairs1
          ~f:(fun (k2,v2) ->
            if k = k2 then assert(lookup r k2 = None)
            else assert(lookup r k2 = Some v2)
          )
        in
        assert(balanced r)
      ) ;
    ()

  let test_remove_reverse_order () =
    let pairs1 = generate_pair_list 26 in
    let d1 = insert_list_reversed empty pairs1 in
    List.iter
      pairs1
      ~f:(fun (k,_) ->
        let r = remove d1 k in
        let _ = List.iter
          pairs1 ~f:(fun (k2,v2) ->
            if k = k2 then assert(lookup r k2 = None)
            else assert(lookup r k2 = Some v2)
          ) in
        assert(balanced r)
      ) ;
    ()

  let test_remove_random_order () =
    let pairs5 = generate_random_list 100 in
    let d5 = insert_list empty pairs5 in
    let r5 = List.fold_right pairs5 ~f:(fun (k,_) d -> remove d k) ~init:d5 in
    List.iter pairs5 ~f:(fun (k,_) -> assert(not (member r5 k))) ;
    assert(r5 = empty) ;
    assert(balanced r5) ;
    () 

  let test_fold () =
    (* Compare fold implementation of size to dict_size function *)
    let pairs = generate_random_list 100 in
    let d1 = insert_list empty pairs in
    let size = fold (fun _ _ a -> 1 + a) 0 d1 in
    assert(dict_size d1 = size) ;
    (* Use fold to empty a dict *)
    let d2 = fold (fun k _ d -> remove d k) d1 d1 in
    assert(d2 = empty) ;
    ()

  let test_lookup () =
    (* Lookup of anything in the empty dict returns None *)
    let pairs1 = generate_random_list 26 in
    List.iter pairs1 ~f:(fun (k,_) -> assert(lookup empty k = None)) ;
    (* Lookup of things we didn't insert returns None *)
    let d1 = insert_list empty pairs1 in
    match List.rev pairs1 with
    | [] -> failwith "pairs list should not be empty"
    | (k,_)::_ ->
        let pairs2 = gen_pairs_from 26 (D.gen_key_gt k ()) in
        List.iter pairs2 ~f:(fun (k,_) -> assert(lookup d1 k = None)) ;
    (* Other cases thoroughly tested through insert & remove tests *)
    ()

  let test_member () =
    (* Nothing is a member of the empty dict *)
    let pairs1 = generate_random_list 26 in
    List.iter pairs1 ~f:(fun (k,_) -> assert(not (member empty k))) ;
    (* Pairs we didn't insert are not members of the dict *)
    let d1 = insert_list empty pairs1 in
    match List.rev pairs1 with
    | [] -> failwith "pairs list should not be empty"
    | (k,_)::_ ->
        let pairs2 = gen_pairs_from 26 (D.gen_key_gt k ()) in
        List.iter pairs2 ~f:(fun (k,_) -> assert(not (member d1 k))) ;
    (* Other cases thoroughly tested through insert & remove tests *)
    ()

  let test_choose () =
    (* Choosing from the empty dict returns None *)
    assert(choose empty = None) ;
    (* Choosing from a dict containing one pair returns that pair and empty *)
    let (k1,v1) = D.gen_pair () in
    let d1 = insert empty k1 v1 in
    assert(choose d1 = Some (k1,v1,empty)) ;
    (* Choosing from a dict returns a dict of size one fewer *)
    let pairs = generate_random_list 100 in
    let d2 = insert_list empty pairs in
    let rec choose_all d =
      match choose d with
      | None -> assert(d = empty) ;
      | Some (_,_,new_d) -> assert((dict_size d) - 1 = dict_size new_d) ;
      choose_all new_d ;
    in
    choose_all d2 ;
    ()

  let run_tests () =
    test_balance() ; 
    test_insert_random() ;
    test_insert_sequential() ;
    test_insert_reversed() ;
    test_insert_identical() ;
    test_remove_nothing() ;
    test_remove_from_nothing() ;
    test_remove_in_order() ;
    test_remove_reverse_order() ;
    test_remove_random_order() ; 
    test_fold();
    test_lookup();
    test_member();
    test_choose();
    ()

end



(******************************************************************)
(* Run our tests.                                                 *)
(******************************************************************)

(* Create a dictionary mapping ints to strings using our
 * BTDict functor and run the tests.
 *
 * Uncomment out the lines below when you are ready to test your
 * 2-3 tree implementation. *)
module IntStringBTDict = BTDict(IntStringDictArg) ;;
IntStringBTDict.run_tests();;




(******************************************************************)
(* Make: a functor that creates a DICT by calling our             *)
(* BTDict functor                                                 *)
(******************************************************************)
module Make (D:DICT_ARG) : (DICT with type key = D.key
  with type value = D.value) =
  BTDict(D) 

