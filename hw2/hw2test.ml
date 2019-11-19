let accept_all string = Some string
let accept_empty_suffix frag = match frag with
   | _::_ -> None
   | x -> Some x

type ling_nonterminals = 
	| Sent | NP | VP | AP | PP | Noun | Verb | Adj | Prep

let ling_grammar = 
	(Sent,
	function
		| Sent -> 
			[[N NP; N VP]]
		| NP ->
			[[N Noun];
			 [N AP; N Noun]]
		| VP ->
			[[N Verb];
			 [N Verb; N NP];
			 [N Verb; N NP; N NP];
			 [N Verb; N NP; N PP];
			 [N Verb; N NP; N NP; N PP]]
		| AP -> 
			[[N Adj]]
		| PP -> 
			[[N Prep; N NP]]
		| Noun ->
			[[T "apple"];
			 [T "Mark"];
			 [T "table"]]
		| Verb ->
			[[T "eats"]]
		| Adj -> 
			[[T "red"]]
		| Prep ->
			[[T "on"]]
);;

let make_matcher_test = ((make_matcher ling_grammar accept_all ["Mark"; "eats"; "red"; "apple"]) = Some ["red"; "apple"])

let frag = ["Mark"; "eats"; "red"; "apple"]
let make_parser_test =
  match make_parser ling_grammar frag with
    | Some tree -> parse_tree_leaves tree = frag
    | _ -> false