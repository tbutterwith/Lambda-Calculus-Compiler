open Core.Std
open Lexer
open Printf
open Simplification
open Lambda_type

let rec parse_channel lexbuf = 
	let output = Parser.main Lexer.read lexbuf in
	match output with
	| Some c 	-> 	print_endline( "" );
					print_endline( "   " ^ lambda_to_string c );
					let alpha = alpha_equiv c [] in 
					print_endline( "=> " ^ lambda_to_string (beta_simp alpha ([],[]) 0));
					print_endline( "" );
					parse_channel lexbuf 
	| None 		-> 	()
			
	

let () = 
	let filename = Sys.argv.(1) in
	let inx = In_channel.create filename in
	let lexbuf = Lexing.from_channel inx in
	lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename }; 
	parse_channel lexbuf;
	In_channel.close inx