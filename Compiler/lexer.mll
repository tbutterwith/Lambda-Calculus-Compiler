{ 
	open Parser
	exception LexError
}

let charList = ['a'-'z' 'A'-'Z']

rule read = parse
	| [' ' '\t']		{ read lexbuf }
	| ['\n' ]        	{ read lexbuf }
	| ['0' - '9']+ as s { INT(int_of_string s) }
	| '\\' 				{ LAMBDA }
	| '.'				{ DOT }
	| "successor"		{ SUCC }
	| "addition"		{ PLUS }
	| '+'				{ PLUS }
	| '-'				{ MINUS }
	| '*'				{ MULT }
	| "multiply"		{ MULT }
	| '/'				{ DIV }
	| '('				{ OPEN }
	| ')'				{ CLOSE }
	| eof				{ EOF }
	| ';'				{ EOL }
	| charList as s 	{ CHAR s}
	| _					{ raise LexError }