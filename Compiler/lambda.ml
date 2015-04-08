open Core.Std

type expr = 
	| Char of char
	| Lambda of char * expr
	| App of expr * expr
	| Closure of expr * expr


let rec lambda_to_string expr = 
	match expr with 
	| Char e 			-> Char.to_string e
	| Lambda (id, e1) 	-> "\\" ^ Char.to_string id ^ "." ^ (lambda_to_string e1) 
	| App (e1, e2)		-> "app [" ^ lambda_to_string e1 ^ "][(" ^ lambda_to_string e2 ^ ")]"
	| Closure (e1, e2)	-> "close " ^ lambda_to_string (App (e1 , e2))

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
	print_endline( "lookup " ^ lambda_to_string (Char x));
 	match variables, values with 
 	| y::yt, z::zt	-> if y = x then z else lookup x (yt,zt)
 	| [], [] 		-> Char x

let rec beta_simp expr stack =
	print_endline ("eval " ^ lambda_to_string expr);
	match expr with 
	| Char e 			-> lookup e stack
	| Lambda (id, e) 	-> Lambda (id, beta_simp e stack)
	| App (e1, e2)		-> 
			(match e1, e2 with
			| Char c, _	->
				let new_c = beta_simp e1 stack in
					(match new_c with
					| Char c 	-> App (e1, e2)
					| _			-> beta_simp (App(new_c, e2)) stack)
			| Lambda (id, e), Char c	-> 
				(match stack with
					| variables, values -> beta_simp e (id::variables, Char c::values))
			| Lambda (id1, expr1), Lambda (id2, expr2) 	->
				(match stack with
				| variables, values 	-> 
					beta_simp expr1 (id1::variables, e2::values))
			| Lambda (id1, exp1), App (exp2, exp3) 	->
				let simp_app = beta_simp e2 stack in
					beta_simp (App (e1, simp_app)) stack)









