type lamda = 
	| Char of char
	| Int of int
	| Lamda of string * lamda
	