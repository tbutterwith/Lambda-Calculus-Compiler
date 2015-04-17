open Core.Std

type expr = 
	| Char of char
	| Lambda of char * expr
	| App of expr * expr
	| Close of expr


let rec lambda_to_string expr = 
	match expr with 
	| Char e 			-> Char.to_string e
	| Lambda (id, e1) 	-> "\\" ^ Char.to_string id ^ "." ^ (lambda_to_string e1) 
	| App (e1, e2)		-> lambda_to_string e1 ^ "(" ^ lambda_to_string e2 ^ ")"
	| Close e			-> lambda_to_string e

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
 	| [], [] 		-> Char x

let rec beta_simp expr stack =
	match expr with 
	| Char e 			-> lookup e stack
	| Lambda (id, e) 	-> Close (Lambda (id, beta_simp e stack))
	| App (e1, e2)		-> 
			(match e1, e2 with
			| Char c, _			->
				let new_c = beta_simp e1 stack in
				 let new_e2 = beta_simp e2 stack in 	
					(match new_c with
					| Char c 	-> Close (App (Char c, new_e2))
					| _			-> beta_simp (App(new_c, e2)) stack)
			| Lambda (id, e), Char c	-> 
				(match stack with
					| variables, values -> beta_simp e (id::variables, Char c::values))
			| Lambda (id1, expr1), Lambda (id2, expr2) 	->
				(match stack with
				| variables, values 	-> 
					beta_simp expr1 (id1::variables, e2::values))
			| Lambda (id1, exp1), App (exp2, exp3) 	->
				let new_expr2 = beta_simp exp2 stack in
					let new_expr3 = beta_simp exp3 stack in
					(match stack with
					| variables, values 	-> beta_simp (App(exp1, new_expr3)) (id1::variables, new_expr2::values) )
			| Close e, Close f -> Close( App( Close e, Close f))
			| _ , Close e ->
				let simp_app = beta_simp e1 stack in
					beta_simp (App (simp_app, Close e)) stack)
	| Close e 		-> Close e









