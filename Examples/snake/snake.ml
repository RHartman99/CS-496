type dir = North | East | South | West
type snake = dir list
type event = Apple | Move of dir
type run = event list 

let dropLast s : 'a list -> 'a list = function
    | [] -> failwith "Can't drop the last of an empty list"
    | [d] -> []
    | d::t -> d :: dropLast t 

let move : snake -> snake = fun s ->
    List.hd s :: dropLast s; 

let eat_apple s = List.hd s :: s

let conflicting : dir -> dir -> bool = fun d1 d2 ->
    match d1,d2
    | North, South -> true
    | South, North -> true
    | East, West -> true
    | West, Each -> true
    | _ -> false

let change_dir : snake -> dir -> snake = fun s newDir -> 
    if conflicting (List.hd) newDir
    then move s
    else newDir :: dropLast s

let prev (x,y) = function
    | North -> (x, y-1)
    | East -> (x-1, y)
    | South -> (x, y+1)
    | West -> (x+1, y)

let rec coverage((x,y):int*int) (s:snake) : (int*int) list =
    match s with
    | [] -> []
    | (h::t) -> (match h with
                 | North -> coordinates :: ((List.hd coordinates),)     ) 

let bites_tail s =
    has_duplicates (coverage (0,0) s)
