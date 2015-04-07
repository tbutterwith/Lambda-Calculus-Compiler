open Core.Std

type expr = 
	| Char of char
	| Lambda of char * expr
	| App of expr * expr


let rec lambda_to_string expr = 
	match expr with 
	| Char e 			-> Char.to_string e
	| Lambda (id, e1) 	-> "(\\" ^ Char.to_string id ^ "." ^ (lambda_to_string e1) ^ ")"
	| App (e1, e2)		-> "app " ^ lambda_to_string e1  ^ lambda_to_string e2	

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
	print_endline( "eval " ^ lambda_to_string expr);
	match expr with
	| Char e 			-> lookup e stack
	| Lambda (id, e1) 	-> Lambda (id, e1)
	| App (e1, e2) 		-> 
		match e1, e2 with
		| Char e, Char f	-> beta_simp e1 stack; beta_simp e2 stack
		| _ , _ 			->
		let simplified_e2 = beta_simp e2 stack in 
			let simplified_e1 = beta_simp e1 stack in
				print_endline ("simp e1 : " ^ lambda_to_string simplified_e1 ^ "  simp e2 : " ^ lambda_to_string simplified_e2);
				match simplified_e1 with
				| Lambda (id, expr1) 	->
					(match stack with
					| variables, values -> beta_simp expr1 (id::variables, simplified_e2::values))
				| Char x 				-> print_endline ("undefined variable " ^ Char.to_string x ^ " in " ^ Char.to_string x ^ lambda_to_string simplified_e2);
											Char x;
							 