{ 
	open Parser
	exception LexError
}

let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_']*

rule read = parse
	| [' ' '\t']		{ read lexbuf }
	| ['\n' ]        	{ EOL }
	| ['0' - '9']+ as s { INT(int_of_string s) }
	| '\\' 				{ LAMBDA }
	| '.'				{ DOT }
	| "plus"			{ PLUS }
	| '+'				{ PLUS }
	| '-'				{ MINUS }
	| '*'				{ MULT }
	| "multiply"		{ MULT }
	| '/'				{ DIV }
	| '('				{ OPEN }
	| ')'				{ CLOSE }
	| id as s			{ STRING s }
	| eof				{ EOF }
	| _					{ raise LexError }