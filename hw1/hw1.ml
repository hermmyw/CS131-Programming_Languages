open List;;
open Printf;;
(* a is a subset of b *)
let rec subset a b = match a with 
  | [] -> true
  | h :: t -> if List.mem h b then (subset t b) else false;;

(* a is equal to b *)
let equal_sets a b = (subset a b) && (subset b a);;

(* a union b *)
let rec set_union a b = match a with 
  | [] -> b  (* if a is empty *)
  | h :: t -> if List.mem h b then set_union t b  (* if current element of a is in b *)
              else h::set_union t b;;   (* if current element of a is not in b *)

(* a intersect b *)
let rec set_intersection a b = match a with
  | [] -> []
  | h :: t -> if List.mem h b then h::set_intersection t b
              else set_intersection t b;;

(* in a not in b *)
let rec set_diff a b = match a with 
  | [] -> []
  | h :: t -> if List.mem h b then set_diff t b
            else h::set_diff t b;;

(* computed_fixed_point *)
let rec computed_fixed_point eq f x = 
  if eq x (f x) then x
  else computed_fixed_point eq f (f x);;   (* go into a loop if no fixed point is found *)

(* filter_reachable *)
type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal;;

type awksub_nonterminals = | Expr | Lvalue | Incrop | Binop | Num;;
 
(*Remove terminals*)
let rec non_terminal l = match l with
    | [] -> []
    | (T _) :: t -> non_terminal t
    | (N h) :: t -> h :: non_terminal t;;

let rec reachable a b = match a with
    | [] -> []
    | h :: t -> match h with
        x,y -> if List.mem x b then (x :: non_terminal y)@reachable t b 
               else reachable t b;;

let filter_reachable_helper g =
    computed_fixed_point (equal_sets) (reachable (Pervasives.snd g)) [Pervasives.fst g];;
    
(*OK, now for the real work. Write a function filter_reachable g that returns a copy of the grammar g with all unreachable rules removed.
* This function should preserve the order of rules: that is, all rules that are returned should be in the same order as the rules in g.*)
let filter_reachable g =
    (Pervasives.fst g, List.filter(fun x -> List.mem(Pervasives.fst x) (filter_reachable_helper g)) (Pervasives.snd g));;




