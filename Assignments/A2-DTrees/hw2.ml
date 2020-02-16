(* Ryan Hartman *)
(* I pledge my honor that I have abided by the Stevens Honor System. *)

type dTree = Node of char * dTree * dTree | Leaf of int

let tLeft =  Node('w', Node('x', Leaf(2), Leaf(5)), Leaf(8))
let tRight = Node('w', Node('x', Leaf(2), Leaf(5)), Node('y', Leaf(7), Leaf(5)))

let bin_func = (['x';'y';'z'], [([0;0;0] , 0); 
                                ([0;0;1] , 1); 
                                ([0;1;0] , 1); 
                                ([0;1;1] , 0); 
                                ([1;0;0] , 1); 
                                ([1;0;1] , 0); 
                                ([1;1;0] , 0); 
                                ([1;1;1] , 1)])

let bin_func2 = [([0;1;1], 5)]

(* Part 1: Basic dTree functions *)

let rec dTree_height : dTree -> int = fun t ->
  match t with
  | Leaf(d) -> 0
  | Node(d, lt, rt) -> 1 + max (dTree_height lt) (dTree_height rt)

let rec dTree_size : dTree -> int = fun t ->
  match t with
  | Leaf(d) -> 1
  | Node(d, lt, rt) -> 1 + dTree_size lt + dTree_size rt

let dTree_paths : dTree -> int list list = fun t ->
  let rec find_paths : dTree -> int list -> int list list = fun t l ->
    match t with
    | Node(d, lt, rt) -> (find_paths lt (l@[0])) @ (find_paths rt (l@[1]))
    | Leaf(d) -> [l] in
  find_paths t []

let rec dTree_is_perfect : dTree -> bool = fun t ->
  match t with
  | Leaf(d) -> true
  | Node(d, lt, rt) -> dTree_height lt = dTree_height rt &&
                        dTree_is_perfect lt && dTree_is_perfect rt

let rec dTree_map : (char -> char) -> (int -> int) -> dTree -> dTree = fun f g t ->
  match t with
  | Leaf(d) -> Leaf(g d)
  | Node(d, lt, rt) -> Node(f d, dTree_map f g lt, dTree_map f g rt)

(* Part 2: Encoding a boolean function *)

let rec list_to_tree : char list -> dTree = fun l ->
  match l with
  | [] -> Leaf(0)
  | h::t -> Node(h, list_to_tree t, list_to_tree t)

let rec parse_bool_func : ((int list) * int) list -> int list -> int -> int = fun f l def  ->
  match f with
  | (func, d)::t -> if func = l then d else parse_bool_func t l def
  | [] -> def

let replace_leaf_at : dTree -> ((int list) * int) list -> dTree = fun t f ->
  let rec replace_leaves : dTree -> ((int list) * int) list -> int list -> dTree = fun t f l ->
    match t with
    | Node(d, lt, rt) -> Node(d, replace_leaves lt f (l@[0]), replace_leaves rt f (l@[1]))
    | Leaf(old) -> Leaf(parse_bool_func f l old) in
  replace_leaves t f []

let bf_to_dTree : ((char list) * (((int list) * int) list)) -> dTree = fun f ->
  match f with
  | (l, func) -> replace_leaf_at (list_to_tree l) func