type dTree = Node of char * dTree * dTree | Leaf of int

let tLeft =  Node('w', Node('x', Leaf(2), Leaf(5)), Leaf(8))
let tRight = Node('w', Node('x', Leaf(2), Leaf(5)), Node('y', Leaf(7), Leaf(8)))

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

let rec replace_leaf : dTree -> (int list) * int -> dTree = fun t f ->
    match f with
    | (h::t, d) -> 
    | ([], d) -> Leaf(d)

let rec replace_leaf_at : dTree -> ((int list) * int) list -> dTree = fun t f ->
    match f with
    | [] -> 
