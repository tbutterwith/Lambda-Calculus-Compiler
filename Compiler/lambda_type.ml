type expr = 
	| Int of int
	| Char of char
	| Lambda of char * expr
	| App of expr * expr
	| Close of expr
	| Error of string