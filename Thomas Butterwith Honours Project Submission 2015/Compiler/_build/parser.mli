type token =
  | INT of (int)
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
  | SUCC
  | ADDITION
  | MULTIPLY

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Lambda_type.expr option
