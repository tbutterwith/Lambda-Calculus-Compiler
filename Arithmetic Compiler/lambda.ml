type lambda = 
	| Char of char
	| Int of int
	| Lambda of string * lambda
	