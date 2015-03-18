open Core.Std
exception Invalid_conversion

type expr = 
	| String of string
	| Lambda of string * expr

let rec lambda_to_string expr = 
	match expr with 
	| String e 				-> e
	| Lambda (id, expr1) 	-> "^" ^ id ^ "." ^ (lambda_to_string expr1)
	