
{
  open Parser;;
  exception Lexical_error;;
}

rule token = parse
    [' ' '\t']  { token lexbuf }
  | "lambda"    { LAMBDA }
  | "L"         { LAMBDA }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "succ"      { SUCC }
  | "pred"      { PRED }
  | "iszero"    { ISZERO }
  | "head"      { HEAD }
  | "tail"      { TAIL }
  | "isnil"     { ISNIL }
  | "cons"      { CONS }
  | "let"       { LET }
  | "letrec"    { LETREC }
  | "in"        { IN }
  | "Bool"      { BOOL }
  | "Nat"       { NAT }
  | "String"    { STRING }
  | "List"      { LIST }
  | "quit"      { QUIT }
  | "as"        { AS }
  | "case"      { CASE }
  | "of"        { OF }
  | "=>"        { EQARROW }
  | "->"        { ARROW }
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | '<'         { LANGLE }
  | '>'         { RANGLE }
  | '.'         { DOT }
  | '='         { EQ }
  | ':'         { COLON }
  | '^'         { CONCAT }
  | '|'         { BAR }
  | '"' [^'"']* '"'
                { STRINGV (String.sub (Lexing.lexeme lexbuf) 1
                  (String.length (Lexing.lexeme lexbuf) - 2)) }
  | '{'         { LBRACE }
  | '}'         { RBRACE }
  | '['         { LBRACKET }
  | ']'         { RBRACKET }
  | ';'         { SEMICOLON }
  | ','         { COMMA }
  | ['0'-'9']+  { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | ['A'-'Z''a'-'z']['a'-'z' '_' '0'-'9']*
                { IDV (Lexing.lexeme lexbuf) }
  | eof         { EOF }
  | _           { raise Lexical_error }

