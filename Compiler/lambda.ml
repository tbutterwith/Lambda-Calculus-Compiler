open Core.Std

type expr = 
	| Char of char
	| Lambda of char * expr
	| App of expr * expr


let rec lambda_to_string expr = 
	match expr with 
	| Char e 			-> Char.to_string e
	| Lambda (id, e1) 	-> "(\\" ^ Char.to_string id ^ "." ^ (lambda_to_string e1) ^ ")"
	| App (e1, e2)		-> lambda_to_string e1  ^ lambda_to_string e2	

let rec expand_church expr = 
	match expr with
	| 0 -> Char 'z'
	| _ -> App (Char 's', (expand_church (expr - 1)) )

let int_to_church expr =
	match expr with
	| 0 	->	Lambda('s', Lambda ('z', Char 'z'))
	| 1		->	Lambda('s', Lambda ('z', App (Char 's', Char 'z')))
	| _		->	Lambda('s', Lambda ('z', (expand_church expr)))

let rec lookup x (variables,values) = 
 	match variables, values with 
 	| y::yt, z::zt	-> if y = x then z else lookup x (yt,zt)


let rec beta_simp expr stack = 
	match expr with
	| Char e 			-> lookup e stack
	| Lambda (id, e1) 	-> Lambda (id, e1)
	| App (e1, e2) 		-> let simplified_e2 = beta_simp e2 stack in 
							let Lambda (id, expr) = beta_simp e1 stack in
							match stack with
							| variables, values -> beta_simp expr (id::variables, simplified_e2::values)
							 