open Core.Std
open Lexer
open Printf
open Lambda

let rec parse_channel lexbuf  expr_list= 
	let output = Parser.main Lexer.read lexbuf in
	match output with
	| Some c -> 
		parse_channel lexbuf (c::expr_list)
	| None -> let simplified = beta_simplification expr_list in 
			print_endline( lambda_to_string simplified)
	

let  () = 
	let filename = Sys.argv.(1) in
	let inx = In_channel.create filename in
	let lexbuf = Lexing.from_channel inx in
	lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename }; 
	parse_channel lexbuf [];
	In_channel.close inx