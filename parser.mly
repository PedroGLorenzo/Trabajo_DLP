
%{
  open Lambda;;
%}

%token LAMBDA
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token SUCC
%token PRED
%token ISZERO
%token LET
%token LETREC
%token IN
%token BOOL
%token NAT
%token STRING
%token QUIT
%token AS
%token CASE
%token OF
%token BAR
%token EQARROW

%token LPAREN
%token RPAREN
%token LANGLE
%token RANGLE
%token DOT
%token EQ
%token COLON
%token ARROW
%token LBRACE
%token RBRACE
%token COMMA
%token CONCAT
%token EOF

%token LIST
%token LBRACKET
%token RBRACKET
%token SEMICOLON

%token HEAD
%token TAIL
%token NIL
%token ISNIL
%token CONS

%token LENGTH
%token APPEND
%token MAP

%token <int> INTV
%token <string> IDV
%token <string> STRINGV

%start s
%type <Lambda.command> s

%%

s :
     IDV EQ ty EOF
       { TypeBind ($1, $3) }
   | IDV EQ term EOF
       { Bind ($1, $3) }
   | term EOF
       { Eval $1 }
   | QUIT EOF
       { Quit }

term :
         appTerm
             { $1 }
     |   term CONCAT term
             { TmConcat ($1, $3) }
     |   IF term THEN term ELSE term
             { TmIf ($2, $4, $6) }
     |   LAMBDA IDV COLON ty DOT term
             { TmAbs ($2, $4, $6) }
     |   LET IDV EQ term IN term
             { TmLetIn ($2, $4, $6) }
     |   LETREC IDV COLON ty EQ term IN term
             { TmLetIn ($2, TmFix (TmAbs ($2, $4, $6)), $8) }
     |   CASE term OF branches
             { TmCase ($2, $4) }

appTerm :
        atomicTerm
            { $1 }
    |   SUCC atomicTerm
            { TmSucc $2 }
    |   PRED atomicTerm
            { TmPred $2 }
    |   ISZERO atomicTerm
            { TmIsZero $2 }
    |   HEAD atomicTerm
            { TmHead $2 }
    |   TAIL atomicTerm
            { TmTail $2 }
    |   ISNIL atomicTerm
            { TmIsNil $2 }
    |   CONS atomicTerm atomicTerm
            { TmCons ($2, $3) }
    |   LENGTH atomicTerm
            { TmLength $2 }
    |   APPEND atomicTerm atomicTerm
            { TmAppend ($2, $3) }
    |   MAP atomicTerm atomicTerm
            { TmMap ($2, $3) }
    |   appTerm atomicTerm
            { TmApp ($1, $2) }
    |   appTerm DOT INTV
            { TmProj ($1, $3) }
    |   appTerm DOT IDV
            { TmProjRcd ($1, $3) }

atomicTerm :
        LPAREN term RPAREN
            { $2 }
    |   TRUE
            { TmTrue }
    |   FALSE
            { TmFalse }
    |   STRINGV
            { TmString $1 }
    |   IDV
            { TmVar $1 }
    |   INTV
            { let rec f = function
                   0 -> TmZero
                |   n -> TmSucc (f (n-1))
            in f $1 }
    |   LBRACE termtuple RBRACE
            { TmTuple $2 }
    |   LBRACKET RBRACKET
            { TmNil TyNat }
    |   LBRACKET termlist RBRACKET
            { $2 }
    |   LBRACE fieldlist RBRACE
            { TmRcd $2 }
    |   LANGLE IDV EQ term RANGLE AS ty
            { TmAs (TmVariant ($2, $4), $7) }

ty :
        atomicTy
            { $1 }
    |   atomicTy ARROW ty
            { TyArr ($1, $3) }

atomicTy :
         LPAREN ty RPAREN
             { $2 }
    |   BOOL
             { TyBool }
    |   NAT
             { TyNat }
    |   STRING
             { TyString }
    |   LBRACKET ty RBRACKET
            { TyList $2 }
    |   LANGLE fieldlist_ty RANGLE
             { TyVariant $2 }
    |   IDV
             { TyName $1 }

termtuple :
        term
            { [$1] }
    |   term COMMA termtuple
            { $1 :: $3 }

fieldlist :
        IDV EQ term
             { [($1, $3)] }
    |   IDV EQ term COMMA fieldlist
             { ($1, $3) :: $5 }

termlist :
        term
            { TmCons ($1, TmNil TyNat) }
    |   term COMMA termlist
            { TmCons ($1, $3) }

fieldlist_ty :
        IDV COLON ty
             { [($1, $3)] }
    |   IDV COLON ty COMMA fieldlist_ty
             { ($1, $3) :: $5 }

branches :
        branch
             { [$1] }
    |   branch BAR branches
             { $1 :: $3 }

branch :
        LANGLE IDV EQ IDV RANGLE EQARROW term
            { ($2, $4, $7) }

