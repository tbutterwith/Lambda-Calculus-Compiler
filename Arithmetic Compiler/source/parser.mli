type token =
  | INT of (int)
  | FLOAT of (float)
  | PLUS
  | MINUS
  | MULT
  | DIV
  | OPEN
  | CLOSE
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> int
