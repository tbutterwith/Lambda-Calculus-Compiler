open Core.Std
open Lexer
open Printf
open Lambda

type test =
{ 	name 	: string ;
	input 	: string ;
	output 	: string ;
}

let test_list = [
	{ name = "numerals";
	  input = "1";
	  output = "\\s.\\z.s(z)";};

	{ name = "single lambda";
	  input = "\\x.x";
	  output = "\\x.x";};

	{ name = "lambda application";
      input = "(\\x.x)(\\y.y)";
	  output = "\\y.y";};

	{ name = "char on char";
      input = "c d";
	  output = "c d";};

	{ name = "char on lambda";
      input = "(\\x.x) c";
	  output = "c";};

	{ name = "lambda on char";
      input = "c (\\x.x)";
	  output = "c(\\x.x)";};

	{ name = "lambda on lambda";
      input = "(\\x.x)(\\y.y)";
	  output = "(\\y.y)";};

	{ name = "successor";
      input = "+";
	  output = "\\m.\\n.\\f.\\x.m(f(n(f(x))))";};

	{ name = "successor application";
	  input = "+ 2 3";
	  output = "\\f.\\x.f(f(f(f(f(x)))))"; };

]

let rec parse_channel lexbuf = 
	let output = Parser.main Lexer.read lexbuf in
	match output with
	| Some c 	-> 	print_endline( "Read as:          " ^ lambda_to_string c );
					print_endline( "Simplified to:    " ^ lambda_to_string (beta_simp c ([],[]) 0));
					print_endline( "" );
					parse_channel lexbuf 
	| None 		-> 	()

let rec parse_list tests = 
	match tests with 
	| [] 	-> ()
	| x::xl -> let test = Lexing.from_string x.input in
				let print_name = print_endline("Test Name:        " ^  x.name ) in
				 let print_input = print_endline ("Input:            "  ^ x.input) in
				  let print_output = print_endline ("Output should be: " ^ x.output) in
				   parse_channel test;
				   parse_list xl

let () = 
	parse_list test_list