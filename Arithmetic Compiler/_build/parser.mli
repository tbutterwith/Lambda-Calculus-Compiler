type token =
  | INT of (int)
  | FLOAT of (float)
  | STRING of (string)
  | CHAR of (char)
  | PLUS
  | MINUS
  | MULT
  | DIV
  | OPEN
  | CLOSE
  | EOL
  | EOF
  | LAMBDA
  | DOT

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Lambda.expr option
