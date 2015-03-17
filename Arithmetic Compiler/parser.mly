%{
	open Lambda
	type expr = Lambda of lambda  
%}


%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token PLUS MINUS MULT DIV OPEN CLOSE EOL EOF LAMBDA DOT

%type <Lambda.expr option> main
%start main

%%

main:
	| EOF					{ None }
	| EOL					{ None }
	| EOL expr				{ Some $2 }
	| expr EOL				{ Some $1 }
	| expr EOF				{ Some $1 }
;

expr:
	| INT 					{ $1 }
	| OPEN expr CLOSE		{ $2 }
	| expr PLUS expr 		{ $1 + $3 }
	| expr MINUS expr		{ $1 - $3 }
	| expr MULT expr 		{ $1 * $3 }
	| expr DIV expr 		{ $1 / $3 }
;
