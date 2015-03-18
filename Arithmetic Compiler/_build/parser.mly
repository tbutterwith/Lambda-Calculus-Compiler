%{
	open Lambda 
	open Core.Std
%}


%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <char> CHAR
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
	| CHAR 						{ String (Char.to_string $1) }
	| STRING 					{ String $1 }
	| LAMBDA STRING DOT expr	{ Lambda ($2, $4) }
	| LAMBDA CHAR DOT expr		{ Lambda ((Char.to_string $2), $4) }
	| OPEN expr CLOSE			{ $2 }	
;
