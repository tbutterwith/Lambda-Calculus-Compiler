{ 
	open Core.Std
	open Parser
	exception Unrecognised_token of string
	exception LexError
}

let charList = ['a'-'z''A'-'Z']
let punc = ['!''"''#''$''%''&''\'''('')''*''+'',''-''/'':''<''=''>''?''@''['']''^''_''`''{''|''}''~']

rule read = parse
	| [' ' '\t' '\n']	{ read lexbuf }
	| ['0' - '9']+ as s { INT(int_of_string s) }
	| '\\' 				{ LAMBDA }
	| '.'				{ DOT }
	| "successor"		{ SUCC }
	| "addition"		{ ADDITION }
	| '('				{ OPEN }
	| ')'				{ CLOSE }
	| eof				{ EOF }
	| ';'				{ EOL }
	| charList as s 	{ CHAR s}
	| punc as s 		{ raise (Unrecognised_token ("unrecognised syntax " ^ Char.to_string s) )}
	| _					{ raise LexError }