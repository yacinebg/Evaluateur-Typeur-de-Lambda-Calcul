%{
  open Ast
%}

%token <int> INT
%token <string> VAR
%token LET IN LAMBDA IFZ IFE THEN ELSE PFIX
%token ADD SUB MULT HEAD TAIL REF DEREF ASSIGN AFFECT COLON SEMICOLON
%token LBRA RBRA LPAREN RPAREN COMMA UNIT EMPTYLIST 
%token EOF

%start main
%type <Ast.pterm> main
%type <Ast.pterm liste> list_expr
%%

main:
  | expr EOF { $1 }

expr:
  | INT                { Int $1 }
  | VAR              { Var $1 }
  | UNIT               { Unit }
  | LAMBDA VAR COLON expr { Abs ($2, $4) }
  | IFZ expr THEN expr ELSE expr { IfZero ($2, $4, $6) }
  | IFE expr THEN expr ELSE expr { IfEmpty ($2, $4, $6) }
  | LET VAR ASSIGN expr IN expr { Let ($2, $4, $6) }
  | PFIX expr         { Pfix $2 }
  | expr ADD expr     { Add ($1, $3) }
  | expr SUB expr     { Sub ($1, $3) }
  | expr MULT expr    { Mul ($1, $3) }
  | HEAD expr         { Head $2 }
  | TAIL expr         { Tail $2 }
  | REF expr          { Ref $2 }
  | DEREF expr        { Deref $2 }
  | expr ASSIGN expr  { Assign ($1, $3) }
  | expr AFFECT expr  { App ($1, $3) }
  | LPAREN expr RPAREN { $2 }
  | EMPTYLIST         { List Vide }
  | LBRA list_expr RBRA { List $2 }
  | expr SEMICOLON expr { App ($1, $3) }

list_expr:
  | expr COMMA list_expr { Cons ($1, $3) }
  | expr                 { Cons ($1, Vide) }
  |                      { Vide }
