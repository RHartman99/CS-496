
(* ******************************************** *)
(** Basic functions on finite automata *)
(* ******************************************** *)

type symbol = char
type input = char list

type state = string

(* transition function *)
type tf = (state * symbol * state) list

(* initial state * transition function * end state *)
type fa = { states: state list; start:state; tf: tf; final: state list}


(* ******************************************** *)
(* Examples of automata *)
(* ******************************************** *)

let a = {states = ["q0";"q1";"q2"];
         start = "q0";
         tf = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")];
         final = ["q2"]}

let a2 = {states = ["q0";"q1";"q2";"q3";"q4"];
          start = "q0";
          tf = [("q0",'a',"q1"); ("q1",'b',"q1")
               ; ("q1",'c',"q2");  ("q3",'a',"q4")];
          final= ["q2"]
         }

let tf_of_a = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")]



(* ******************************************** *)
(* Helper functions *)
(* ******************************************** *)

let input_of_string s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []


(* ******************************************** *)
(* Simulating automata *)
(* ******************************************** *)

    
let rec apply_transition_function : tf -> state -> symbol -> state option  = fun f st sym ->
  match f with
  | [] -> None
  | (q1, tsym, q2)::t -> if q1 = st && sym = tsym then Some q2 else apply_transition_function t st sym


let accept : fa -> input -> bool = fun m word ->
 let rec run : fa -> input -> state -> bool = fun m word st ->
  match word with
  | [] -> if List.mem st m.final then true else false
  | h::t -> (match apply_transition_function m.tf st h with
             | None -> false
             | Some nst -> run m t nst) in
 run m word m.start

let rec next : tf -> state -> symbol -> state list = fun f st sym ->
  match f with
  | [] -> []
  | (q1, tsym, q2)::t -> if q1 = st && sym = tsym then q2::(next t st sym) else next t st sym

(* ************************************************** *)
(* Gets all possible next states from a certain state *)
(* ************************************************** *)

let rec next' : tf -> state -> symbol list -> state list = fun f st l ->
  match l with
  | [] -> []
  | sym::t -> (next f st sym) @ (next' f st t)

let rec alphabet : tf -> symbol list = fun f ->
  match f with 
  | [] -> []
  | (q1, tsym, q2)::t -> List.sort_uniq compare ([tsym] @ alphabet t)

let rec deterministic_t : symbol list -> state -> tf -> bool = fun alph st f ->
  match alph with
  | [] -> true
  | h::t -> if List.length (next f st h) > 1 then false else deterministic' t st f

let deterministic : fa -> bool = fun m ->
  let rec run : fa -> state list -> bool = fun m states ->
    match states with
    | [] -> true
    | st::t -> if deterministic_t (alphabet m.tf) st m.tf then run m t else false in
  run m m.states
  

let valid : fa -> bool = fun m ->
  if  List.mem m.start m.states &&
      List.for_all (fun elem -> List.mem elem m.states) m.final &&
      deterministic m then true
  else false

let reachable : fa -> state list = fun m ->
  let rec run : fa -> state list -> state list -> state list = fun m l visited ->
    match l with
    | [] -> [m.start]
    | st::t -> if List.mem st visited then 
                run m t visited 
               else 
                List.sort_uniq compare @@ st :: run m (t @ next' m.tf st @@ alphabet m.tf) (st::visited) in
  run m [m.start] []
  

let remove_dead_states : fa -> fa = fun m ->
  {states=(reachable m);start=(m.start);tf=(m.tf);final=(m.final)}