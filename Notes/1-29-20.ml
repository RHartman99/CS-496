(* Algebraic Data Types *)

type prim_color = Red | Green | Blue

let next:prim_color -> prim_color = fun c -> 
  match c with
  | Red -> Green
  | Green -> Blue
  | Blue -> Red

type btreei = Empty | Node of int*btreei*btreei

let t1 = Node(40,
              Node(20, Empty, Empty),
              Node(70,
                    Node (44, Empty, Empty),
                    Empty))

let rec sizet = function
  | Empty -> 0
  | Node(i,lt,rt) -> 1 + sizet lt + sizet rt

let rec sumt = function
  | Empty -> 0
  | Node(i, lt, rt) -> i+sumt lt + sumt rt

let rec bump = function
  | Empty -> Empty
  | Node(i,lt,rt) -> Node(i+1, bump lt, bump rt)

(* Polymorphic tree *)
type 'a btree = Empty | Node of 'a * 'a btree * 'a btree

let rec mapt: ('a -> 'b) -> 'a btree -> 'b btree = fun f t ->
  match t with
  | Empty -> Empty
  | Node(i, lt, rt) -> Node(f i, mapt f lt, mapt f rt)

let rec foldt a f t =
  match t with
  | Empty -> a
  | Node(i, lt, rt) = f i (foldt a f lt) (foldt a f rt)