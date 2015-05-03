open Core.Std

type expr = 
	| Int of int
	| Char of char
	| Lambda of char * expr
	| App of expr * expr
	| Close of expr
	| Error of string

let succ 	= Lambda ( 'n', Lambda ('s', Lambda('z', App( Char 's', App(Char 'n', App (Char 's', Char 'z'))))))
let add		= Lambda ( 'm', Lambda ('n', Lambda ('f', Lambda ('x', App(Char 'm', App(Char 'f', App(Char 'n', App(Char 'f', Char 'x'))))))))
let mult 	= Lambda ( 'p', Lambda ('q', App( Char 'q', App(add, App( Char 'p', Lambda ('s', Lambda ('z', Char 'z')))))))

let alpha_list = ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z';]

let rec lambda_to_string expr = 
	match expr with 
	| Int i 			-> string_of_int i
	| Char e 			-> Char.to_string e
	| Lambda (id, e1) 	-> "\\" ^ Char.to_string id ^ "." ^ (lambda_to_string e1) 
	| App (e1, e2)		-> lambda_to_string e1 ^ "(" ^ lambda_to_string e2 ^ ")"
	| Close e			-> lambda_to_string e
	| Error e 			-> e

let rec lambda_to_string_annotate expr = 
	match expr with 
	| Char e 			-> Char.to_string e
	| Lambda (id, e1) 	-> "\\" ^ Char.to_string id ^ "." ^ (lambda_to_string e1) 
	| App (e1, e2)		-> "a {" ^ lambda_to_string e1 ^ "}{(" ^ lambda_to_string e2 ^ ")}"
	| Close e			-> "c [" ^ lambda_to_string e ^ "]"
	| Error e 			-> e

let rec expand_church expr = 
	match expr with
	| 0 -> Char 'z'
	| _ -> App (Char 's', (expand_church (expr - 1)) )

let int_to_church expr =
	match expr with
	| 0 	->	Lambda('s', Lambda ('z', Char 'z'))
	| 1		->	Lambda('s', Lambda ('z', App (Char 's', Char 'z')))
	| _		->	Lambda('s', Lambda ('z', (expand_church expr)))

let rec remove_closed expr = 
	match expr with
	| Char c 			-> Char c
	| Lambda(id, e)		-> Lambda (id, remove_closed e)
	| App(e1, e2)		-> let exp1 = remove_closed e1 in
							let exp2 = remove_closed e2 in
							 App (exp1, exp2)
	| Close e 			-> remove_closed e

let rec combine_apps expr =
	match expr with
	| App (e1, e2) 		->
		(match e1, e2 with
			| App (ex1, ex2), App (ex3, ex4) 	-> App(combine_apps ex1, combine_apps (App (ex2, App(ex3, ex4))))
			| App (ex1, ex2), _ 				-> App(combine_apps ex1, combine_apps (App (ex2, e2)))
			| _ , App (ex1, ex2) 				-> App(e1, combine_apps e2)
			| _, _ 								-> App (e1, e2))
	| Lambda (id, e)	-> Lambda (id, combine_apps e)
	| Char c 			-> expr

let rec lookup_id id used = 
	match used with
	| [] 		-> false
	| x::xt 	-> if x = id then true else (lookup_id id xt)

let rec replace_id expr id replacement = 
	match expr with
	| Char c 			-> if c = id then Char replacement else Char c
	| Lambda (i, e)		-> if i = id then Lambda (replacement, (replace_id e id replacement)) else Lambda (i, (replace_id e id replacement))
	| App (e1, e2)		-> App( replace_id e1 id replacement, replace_id e2 id replacement)

let rec find_replacement_id used alpha =
	match alpha with
	| [] 	-> 'a'	
	| x::xt -> if (lookup_id x used) = false then x else find_replacement_id used xt

let rec get_used_ids expr used = 
	match expr with
	| Char c 		-> c::used
	| Lambda(id, e) -> id::(get_used_ids e used)
	| App (e1, e2) 	-> (get_used_ids e1 used)@(get_used_ids e2 used)

let rec alpha_equiv expr taken = 
	match expr with
	| Int i 			-> Int i
	| Char c 			-> Char c
	| Lambda (id, e)  	-> if (lookup_id id taken) = false then Lambda (id, (alpha_equiv e (id::taken))) else 
								let new_id = find_replacement_id taken alpha_list in
								  let new_lambda = Lambda (new_id, (replace_id e id new_id)) in
								   alpha_equiv new_lambda taken
	| App(e1, e2)		-> let alpha_e1 = alpha_equiv e1 taken in
	 						let alpha_e2 = alpha_equiv e2 (get_used_ids alpha_e1 taken) in
	 						 App (alpha_e1, alpha_e2)

let rec lookup x (variables,values) = 
 	match variables, values with 
 	| y::yt, z::zt	-> if y = x then z else lookup x (yt,zt)
 	| [], [] 		-> Char x

let rec beta_simp expr stack steps=
	match steps with 
	| 200 	-> expr
	| _		->
		match expr with
		| Int i 			-> Int i 
		| Char e 			-> lookup e stack
		| Lambda (id, e) 	-> let Lambda(id2, e2) = combine_apps expr in
								Close (Lambda (id2, beta_simp e2 stack (steps+1)))
		| App (e1, e2)		-> 
				(match e1, e2 with
				| Char c, _			->
					let new_c = beta_simp e1 stack (steps+1) in
					 let new_e2 = beta_simp e2 stack (steps+1) in 	
						(match new_c with
						| Char c 	-> Close (App (Char c, new_e2))
						| _			-> beta_simp (App(new_c, e2)) stack (steps+1))
				| Lambda (id, e), Char c	-> 
					(match stack with
						| variables, values -> beta_simp e (id::variables, Char c::values) (steps+1))
				| Lambda (id1, expr1), Lambda (id2, expr2) 	->
					(match stack with
					| variables, values 	-> 
						let simp_lamb = beta_simp expr1 (id1::variables, e2::values) (steps+1) in
						 beta_simp (remove_closed simp_lamb) (id1::variables, e2::values) (steps+1))
				| Lambda (id1, exp1), App (exp2, exp3) 	->
						(match stack with
						| variables, values 	-> 
							let simp_e1 = beta_simp exp1 (id1::variables, exp2::values) (steps+1) in
							beta_simp (App(simp_e1, exp3)) (id1::variables, exp2::values) (steps+1))
				| Lambda (id, exp), Close e ->
					(match stack with
					| variables, values 	-> 
						beta_simp exp (id::variables, e::values) (steps+1))
				| App (expr1, expr2), App (expr3, expr4) ->
					let simp_e1 = beta_simp (App (expr1, expr2)) stack (steps+1) in
						let simp_e2 = beta_simp (App (expr3, expr4)) stack (steps+1) in
						beta_simp (App (simp_e1, simp_e2)) stack (steps+1)
				| App (expr1, expr2), _ -> 
					let simp_e1 = beta_simp e1 stack (steps+1) in
					 beta_simp (App (simp_e1, e2)) stack (steps+1)
				| Close e, Close f -> Close( App( Close e, Close f))
				| _ , Close e ->
					let simp_app = beta_simp e1 stack (steps+1) in
						beta_simp (App (simp_app, Close e)) stack (steps+1)
				| Close e, _ -> let rem_clo = remove_closed e in
									beta_simp (App (rem_clo, e2)) stack (steps+1))
		| Close e 		-> Close e









