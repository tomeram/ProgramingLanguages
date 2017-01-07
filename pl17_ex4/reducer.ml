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
(* fv : term -> StringSet.t *)
let rec fv = function
	| Variable v -> StringSet.add v StringSet.empty
	| Abstraction (id, t) -> StringSet.remove id (fv t)
	| Application (t1, t2) -> StringSet.union (fv t1) (fv t2)


(* fresh_var : StringSet.t -> string *)
let fresh_var variables_in_use =
	let diff = StringSet.diff (string_set_from_list possible_variables) variables_in_use in
	(
		let n = StringSet.is_empty diff in
		(
		match n with
		| false -> List.nth (StringSet.elements diff) 0
		| true -> raise (OutOfVariablesError)
		)
	)

(* substitute : string -> term -> term -> term *)
let rec substitute x t1 t2 =
	match t2 with
	| Variable v -> let parsed_x = (Variable x) in 
		(
		match parsed_x with
		| (Variable v) -> t1
		| _ -> t2
		)
	| Application (s1,s2) -> (Application ((substitute x t1 s1),(substitute x t1 s2)))
	| Abstraction (id,t) -> (let parsed_x = (Variable x) in 
		match parsed_x with
		| (Variable id) -> t2
		| _ -> let free_vars = fv t1 in
			(
			let belongs = StringSet.mem id free_vars in
				(
				match belongs with
				| false -> (Abstraction ( id,(substitute x t1 t)))
				| true -> let z = fresh_var (StringSet.union (fv t) (fv t1)) in 
				Abstraction ( z,(substitute x t1 (substitute id (Variable z) t)))
				)
			)
		)