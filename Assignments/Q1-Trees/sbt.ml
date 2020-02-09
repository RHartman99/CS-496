
type 'a btree = Empty | Node of 'a * 'a btree * 'a btree

let t1 = Node(30,Node(20,Node(10,Empty,Empty),Empty),Empty)
let t2 = Node(4,
              Node(3,
                   Node(2,
                        Node(1,Empty,Empty),
                        Empty),
                   Empty),
              Node(5,
                   Empty,
                   Node(6,
                        Empty,
                        Node(7,Empty,Empty))))

let t3 = Node(12,
              Node(7,Empty,Empty),
              Node(24,
                   Node(18,Empty,Empty),
                   Empty))
    
(** A list with the i-the level of t. 1 is the first level. 
   If the level is greater than the height of the tree, then 
   fail with failwith 
*)
let rec level t i =
  match t with
  | Node(v, l, r) -> if i = 0 then [v] else level l (i-1) @ level r (i-1)
  | Node(v, l, Empty) -> if i = 0 then [v] else level l (i-1)
  | Node(v, Empty, r) -> if i = 0 then [v] else level r (i-1)
  | Node(v, Empty, Empty) -> if i = 0 then [v] else []
  | Empty -> []
  

(** A list of lists with all the levels of the tree. 
    Return the empty list if the tree is Empty *)
let rec height t =
  match t with
  | Empty -> 0
  | Node(v, l, r) -> 1 + max (height l) (height r)

let rec levels t =
  match t with
  | Empty -> []
  | Node(v, l, r) -> v @ levels l @ levels r


(** Perfect binary tree of a given height whose nodes contain d as
    data. The height is h is an integer greater or equal to zero.
*)
let rec pbt h d =
  match h with
  | 0 -> Empty
  | a -> Node(d, pbt (a-1) d, pbt (a-1) d)

(** A list with all the paths from the root to a leaf 
    eg: 
    # paths_to_leaves t2;;
    - : int list list = [[0; 0; 0]; [1; 1; 1]] 
*)      
let rec paths_to_leaves t =
  failwith "implement"
                         
(** A list with all the paths from the root to any node. 
    eg: 
    # paths t2;;
    - : int list list = [[0; 0; 0]; [0; 0]; [0]; [1; 1; 1]; [1; 1];
    [1]; []]
    
    If the tree is empty, then paths returns the empty list []
*)  
let rec paths t =
  failwith "implement"

