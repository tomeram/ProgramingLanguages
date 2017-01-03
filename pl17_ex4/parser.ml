(*
  Parser for lambda-calculus.
*)

open Utils
open Lexer


(* AST for lambda expressions - DO NOT MODIFY THIS TYPE *)
type term = | Variable of string
	    | Abstraction of string * term
	    | Application of term * term

(*
  Concrete Syntax:
  t ::= id | (\id.t) | (t1 t2) | (t) | let id=t1 in t2

  Abstract Syntax:
  term ::= id | \id.term | term1 term2
*)

exception SyntaxError of string

(*
  ADD FUNCTIONS BELOW
*)

 (*parse_term*)
let rec parse_term = function
	| (Literal c) :: ts -> (Variable c, ts) (* id *)
	| LParen :: LambdaTok :: (Literal c) :: DotTok :: ts -> let (t, ts1) = parse_term ts in 
		(
		match ts1 with
			| RParen :: ts2 -> (Abstraction(c,t),ts2); (* (\id.t) -- abstraction *)
			| _ -> raise (syntax_error "Right-parenthesis expected 1.\n")
		)
	| LParen :: ts -> let (t, ts1) = parse_term ts in
		(
		match ts1 with
			| RParen :: ts2 -> (t, ts2) (* (t) *)
			| ts3 -> let (t1, ts4) = parse_term ts3 in
				(
				match ts4 with
					| RParen :: ts5 -> (Application(t,t1),ts5) (* (t1 t2) -- application *)
					| _ -> raise (syntax_error "Right-parenthesis expected 2.\n")
				)
			| _ -> raise (syntax_error "Unexpected token_1.\n")
		)
	| LetTok :: (Literal c) :: EqTok :: ts -> let (t, ts1) = parse_term ts in
		(
		match ts1 with
		| InTok :: ts2 -> let (t1, ts3) = parse_term ts2 in
			(
			Application(Abstraction(c,t1),t),ts3 (* let id=t1 in t2 *)
			)
		| _ -> raise (syntax_error "In token expected.\n")
		)
	| _ -> raise (syntax_error "Unexpected token_2.\n")
  
 (*parse*)
let parse s = let (t, ts) =
		try s |> String.to_list |> tokenize |> parse_exp with
		  LexicalError -> raise (SyntaxError "Illegal input.\n")
	      in
	      match ts with
	      | [] -> t
	      | _ -> raise (SyntaxError "Unexpected input.\n")
  
 (*format_term*)
let rec format_term = function
  | Variable c -> "(" ^ c ^ ")"
  | Abstraction (id, t) -> "(" ^ "\\" ^ id ^ ". " ^ (format_term(t)) ^ ")"
  | Application (t1, t2) -> "(" ^ (format_term(t1)) ^ " " ^ (format_term(t2)) ^ ")"