{ 
	open Parser
	exception LexError
}

let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_']*
let charList = ['a'-'z' 'A'-'Z']

rule read = parse
	| [' ' '\t']		{ read lexbuf }
	| ['\n' ]        	{ EOL }
	| ['0' - '9']+ as s { INT(int_of_string s) }
	| '\\' 				{ LAMBDA }
	| charList as s 	{ CHAR s}
	| '.'				{ DOT }
	| "plus"			{ PLUS }
	| '+'				{ PLUS }
	| '-'				{ MINUS }
	| '*'				{ MULT }
	| "multiply"		{ MULT }
	| '/'				{ DIV }
	| '('				{ OPEN }
	| ')'				{ CLOSE }
	| eof				{ EOF }
	| _					{ raise LexError }