type expr = 
	| Char of char
	| Int of int
	| Lambda of string * expr
	