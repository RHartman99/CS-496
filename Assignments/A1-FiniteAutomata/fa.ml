(* Ryan Hartman *)
(* I pledge my honor that I have abided by the Stevens Honor System. *)

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
          tf = [("q0",'a',"q1"); ("q3",'a',"q4"); ("q1",'b',"q1");
                ("q1",'c',"q2") ];
          final= ["q2"; "q3"]
         }

let tf_of_a = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")]



(* ******************************************** *)
(* Helper functions *)
(* ******************************************** *)

let input_of_string s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

(* Returns a symbol list containing all symbols in the alphabet 
 * of a transition function                                    *)
let rec alphabet : tf -> symbol list = fun f ->
  match f with 
  | [] -> []
  | (q1, tsym, q2)::t -> List.sort_uniq compare (tsym :: alphabet t)


(* ******************************************** *)
(* Simulating automata *)
(* ******************************************** *)

(* Returns the destination state as a result of a transition function, 
 * or None if the fuction is not defined                            *)    
let rec apply_transition_function : tf -> state -> symbol -> state option  = fun f st sym ->
  match f with
  | [] -> None
  | (q1, tsym, q2)::t -> if q1 = st && sym = tsym then Some q2 else apply_transition_function t st sym

(* Determines if a machine will accept an input string *)
let accept : fa -> input -> bool = fun m word ->
 let rec run : fa -> input -> state -> bool = fun m word st ->
  match word with
  | [] -> if List.mem st m.final then true else false
  | h::t -> match apply_transition_function m.tf st h with
             | None -> false
             | Some nst -> run m t nst in
 run m word m.start

(* Finds all possible next states given a state and a single symbol *)
let rec next : tf -> state -> symbol -> state list = fun f st sym ->
  match f with
  | [] -> []
  | (q1, tsym, q2)::t -> if q1 = st && sym = tsym then q2::(next t st sym) else next t st sym

(* Takes a transition function, state and symbol list and finds all possible
 * states that can be reached from the given state.                        *)
let rec next_l : tf -> state -> symbol list -> state list = fun f st l ->
  match l with
  | [] -> []
  | sym::t -> (next f st sym) @ (next_l f st t)

(* Evaluates if a transition function is deterministic for a given state
   for all symbols in a list                           *)
let rec deterministic_t : symbol list -> state -> tf -> bool = fun l st f ->
  match l with
  | [] -> true
  | h::t -> if List.length (next f st h) > 1 then false else deterministic_t t st f
  

(* Evaluates if a machine is deterministic (no state has two transition function 
   definitions involving the same symbol)                                             *)
let deterministic : fa -> bool = fun m ->
  let alph = alphabet m.tf in
  let rec run : fa -> state list -> bool = fun m states ->
    match states with
    | [] -> true
    | st::t -> if deterministic_t alph st m.tf then 
                run m t 
              else 
                false in
  run m m.states

(* Determines if a finite automata is valid; the start state is in its list of states,
   all accept states are in its list of states, and it is deterministic.             *)
let valid : fa -> bool = fun m ->
  List.mem m.start m.states &&
  List.for_all (fun elem -> List.mem elem m.states) m.final &&
  deterministic m

(* Returns a list of states in a finite automata that are reachable from the start state. *)
let reachable : fa -> state list = fun m ->
  let alph = alphabet m.tf in
  let rec run : fa -> state list -> state list -> state list = fun m l visited ->
    match l with
    | [] -> []
    | st::t -> if List.mem st visited then 
                run m t visited 
               else 
                (* If we haven't visited this state yet, append it and recurse,
                   adding the state to the list of visited states. *)
                List.sort_uniq compare @@ st :: run m (t @ next_l m.tf st alph) (st::visited) in
  run m [m.start] []

(* Removes transitions in a transition function having
   a non reachable state as its destination.
   f = transition function
   l = list of reachable states of tbe machine *)
let rec clean_tf : tf -> state list -> tf = fun f l ->
    match f with
    | [] -> []
    | (q1, tsym, q2)::t -> if List.mem q1 l then 
                              (q1, tsym, q2) :: clean_tf t l 
                           else 
                              clean_tf t l   

let remove_dead_states : fa -> fa = fun m ->
  let reachable_states = reachable m in
  let cleaned_tf = clean_tf m.tf reachable_states in
  let cleaned_final = List.filter (fun st -> List.mem st reachable_states) m.final in
  {states=reachable_states; start=m.start; tf=cleaned_tf; final=cleaned_final}
  