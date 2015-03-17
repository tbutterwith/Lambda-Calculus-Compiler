open Core.Std
open Lexer

let rec parse_channel lexbuf = 
	let output = Parser.main Lexer.read lexbuf in
		print_int output; print_newline(); flush stdout;
	parse_channel lexbuf

let  () = 
	let filename = Sys.argv.(0) in
	let inx = In_channel.create filename in
	let lexbuf = Lexing.from_channel inx in
	lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename }; 
	parse_channel lexbuf;
	In_channel.close inx