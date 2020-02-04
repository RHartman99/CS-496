let rec fact (n:int):int =
  match n with
  | 0 -> 1
  | m -> m * fact(m-1)

let rec sum n (n:int):int =
  match n with
  | 0 -> 0
  | m when m>0 -> m + sum (m+1)
  | _ -> failwith "Not defined on negative numbers"

let rec sizel l =
  match l with
  | [] -> 0
  | h::t -> 1 + sizel t