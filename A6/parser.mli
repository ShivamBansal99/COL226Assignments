type token =
  | COLON
  | EQUAL
  | LP
  | RP
  | COMMA
  | CALL
  | EOL
  | EOF
  | RET
  | INT of (int)
  | ID of (string)

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Expression.expr_tree
