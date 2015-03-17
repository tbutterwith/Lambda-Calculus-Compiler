{ 
	open Parser
	exception LexError
}

rule read = parse
	| [' ' '\t']		{ read lexbuf }
	| ['\n' ]        	{ EOL }
	| ['0' - '9']+ as s { INT(int_of_string s) }
	| ['a' - 'z']+ as s { STRING s }
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