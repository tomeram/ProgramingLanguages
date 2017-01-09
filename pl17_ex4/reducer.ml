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
	| Variable v -> StringSet.singleton v
	| Abstraction (id, t) -> StringSet.remove id (fv t)
	| Application (t1, t2) -> StringSet.union (fv t1) (fv t2)


(* fresh_var : StringSet.t -> string *)
let fresh_var variables_in_use =
	let diff = StringSet.diff (string_set_from_list possible_variables) variables_in_use in
	(
		let n = StringSet.is_empty diff in
		(
		match n with
		| false -> StringSet.choose diff
		| true -> raise (OutOfVariablesError)
		)
	)

(* substitute : string -> term -> term -> term *)

let rec substitute x t1 t2 =
	match t2 with
	| Variable v -> (match v with
		| q when q = x -> t1
		| _ -> t2
		)
	| Application (s1,s2) -> (Application ((substitute x t1 s1),(substitute x t1 s2)))
	| Abstraction (id,t) -> (match id with
		| w when w = x -> t2 (* if x = y *)
		| _ -> let free_vars = fv t1 in
			(
			let belongs = StringSet.mem id free_vars in
				(
				match belongs with
				| false -> (Abstraction ( id,(substitute x t1 t))) (* if y <> x and y not belongs to fv(s) *)
				| true -> let z = fresh_var (StringSet.union (fv t) (fv t1)) in (* if y <> x and y belongs to fv(s) *)
				Abstraction ( z,(substitute x t1 (substitute id (Variable z) t)))
				)
			)
		)

(*
	let rec substitute x t1 t2 = 

	match t2 with
	| Variable v -> if x=v then t1 else t2   
	| Application (t1',t2') -> (Application ((substitute x t1 t1') , (substitute x t1 t2')))
	
	| Abstraction (y, t) -> 	
		(
			if (x=y)
				then t2
			else if (not (StringSet.exists (eq y) (fv t1))) 
				then (Abstraction( y, (substitute x t1 t)))
			else 
				let z = fresh_var(StringSet.union (fv t1) (fv t)) in Abstraction( z, (substitute x t1 (substitute y (Variable z) t) ) )
		)
*)

(* reduce_strict : term -> term option *)
let rec reduce_strict = function
	| Application (t1, t2) -> let reduced_t1 = reduce_strict(t1) in
		(
		match reduced_t1 with
		| Some reduced_t1' -> Some(Application(reduced_t1', t2)) (* E-App1 *)
		| None -> let reduced_t2 = reduce_strict(t2) in
			(
			match reduced_t2 with
			| Some reduced_t2' -> Some(Application(t1, reduced_t2')) (* E-App2 *)
			| None -> (match t1 with
				| Abstraction(x,t12) -> Some(substitute x t2 t12) (* E-AppAbs *)
				| _ -> None
				)
			)
		)
	| _ -> None

(* reduce_lazy : term -> term option *)
let rec reduce_lazy = function
	| Application (t1, t2) -> (match t1 with 
		| Abstraction(x,t12) -> Some(substitute x t2 t12) (* E-AppAbs *)
		| _ -> let reduced_t1 = reduce_lazy(t1) in
			(
			match reduced_t1 with
			| Some reduced_t1' -> Some(Application(reduced_t1', t2)) (* E-App1 *)
			| None -> None
			)
		)
	| _ -> None