(*
  Reducers (interpreters) for lambda-calculus.
*)

open Utils
open Parser


exception OutOfVariablesError


let possible_variables = List.map (fun x -> char_to_string (char_of_int x)) ((range 97 123) @ (range 65 91))



(*
  ADD FUNCTIONS BELOW
*)
let rec fv = function
	| Variable v -> StringSet.add v (StringSet.empty)
	| Abstraction (id, t) -> StringSet.remove id (fv t)
	| Application (t1, t2) -> StringSet.union (fv t1) (fv t2)


let fresh_var variables_in_use =
	let diff = StringSet.diff (string_set_from_list possible_variables) variables_in_use in
	(
		let n = StringSet.is_empty diff in
		(
		match n with
		| true -> raise (OutOfVariablesError)
		| false -> List.nth (StringSet.elements diff) 0
		)
	)