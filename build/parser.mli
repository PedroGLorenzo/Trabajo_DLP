type token =
  | LAMBDA
  | TRUE
  | FALSE
  | IF
  | THEN
  | ELSE
  | SUCC
  | PRED
  | ISZERO
  | LET
  | LETREC
  | IN
  | BOOL
  | NAT
  | LPAREN
  | RPAREN
  | DOT
  | EQ
  | COLON
  | ARROW
  | EOF
  | INTV of (int)
  | IDV of (string)

val s :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Lambda.term
