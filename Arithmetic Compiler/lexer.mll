{ 
	open Parser
	exception LexError
}

let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read = parse
	| [' ' '\t']		{ read lexbuf }
	| ['\n' ]        	{ EOL }
	| ['0' - '9']+ as s { INT(int_of_string s) }
	| id as s		{ STRING s }
	| '^' 				{ LAMBDA }
	| '.'				{ DOT }
	| '+'				{ PLUS }
	| '-'				{ MINUS }
	| '*'				{ MULT }
	| '/'				{ DIV }
	| '('				{ OPEN }
	| ')'				{ CLOSE }
	| eof				{ EOF }
	| _					{ raise LexError }