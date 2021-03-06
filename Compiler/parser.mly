%{
	open Simplification
	open Lambda_type
	open Lib 
	open Core.Std
	exception Unrecognised_syntax of string
%}


%token <int> INT
%token <char> CHAR
%token PLUS MINUS MULT DIV OPEN CLOSE EOL EOF LAMBDA DOT SUCC ADDITION MULTIPLY

%type <Lambda_type.expr option> main
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
	| INT 						{ int_to_church $1 }
	| CHAR 						{ Char $1 }
	| expr expr					{ App ($1, $2) }
	| LAMBDA CHAR DOT expr		{ Lambda ($2, $4) }
	| OPEN expr CLOSE			{ $2 }
	| SUCC 						{ succ }
	| ADDITION					{ add }
;
