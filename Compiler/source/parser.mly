%{type expr = Int of int  %}

%token <int> INT
%token <float> FLOAT
%token PLUS MINUS MULT DIV OPEN CLOSE EOF

%start main
%type <int> main

%%

main:
	| expr EOF		{ $1 }
;

expr:
| INT 				{ $1 }
| OPEN expr CLOSE	{ $2 }
| expr PLUS expr 	{ $1 + $3 }
| expr MINUS expr	{ $1 - $3 }
| expr MULT expr 	{ $1 * $3 }
| expr DIV expr 	{ $1 / $3 }
| eof            	{ raise Eof }
;
