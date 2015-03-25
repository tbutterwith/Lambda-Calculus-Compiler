{ 
	open Parser
	exception Eof
}

rule read = parse
	| [' ' '\n' '\t']	{ read lexbuf }
	| ['0' - '9']+ as s { INT(int_of_string s) }
	| "plus"			{ PLUS }
	| "minus"			{ MINUS }
	| "mult"			{ MULT }
	| "div"				{ DIV }
	| '('				{ OPEN }
	| ')'				{ CLOSE }
	| eof				{ EOF }