open Core.Std

type expr = 
	| String of string
	| Lambda of string * expr
	| App of expr * expr

let succ 	= Lambda ( "nfx", String "f(nfx)" )
let add		= Lambda ( "mnfx", String "mf(nfx)" )
let mult	= Lambda ( "mnfx", String "m(nf)x" )


let rec lambda_to_string expr = 
	match expr with 
	| String e 				-> e
	| Lambda (id, expr1) 	-> "\\" ^ id ^ "." ^ (lambda_to_string expr1)
	| App (expr1, expr2)	-> "(" ^ lambda_to_string expr1 ^ ")" ^	"(" ^ lambda_to_string expr2 ^ ")"	

let rec expand_church expr = 
	match expr with
	| 0 -> ""
	| 1 -> "s(z)"
	| _ -> "s(" ^ (expand_church (expr - 1)) ^ ")"

let int_to_church expr = 
	match expr with 
	| 0 -> Lambda ( "sz", String "z" )
	| 1 -> Lambda ( "sz", String "s(z)" )
	| _ -> Lambda ( "sz", String ( expand_church expr )) 

