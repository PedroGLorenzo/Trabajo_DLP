
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
  | "let"       { LET }
  | "letrec"    { LETREC }
  | "in"        { IN }
  | "Bool"      { BOOL }
  | "Nat"       { NAT }
  | "String"    { STRING }
  | "quit"      { QUIT }
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | '.'         { DOT }
  | '='         { EQ }
  | ':'         { COLON }
  | "->"        { ARROW }
  | '^'         { CONCAT }
  | '"' [^ '"']* '"'  { STRINGV (let str = Lexing.lexeme lexbuf in 
                        String.sub str 1 (String.length str - 2)) }
  | ['0'-'9']+  { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | ['a'-'z']['a'-'z' '_' '0'-'9']*
                { IDV (Lexing.lexeme lexbuf) }
  | eof         { EOF }
  | _           { raise Lexical_error }

