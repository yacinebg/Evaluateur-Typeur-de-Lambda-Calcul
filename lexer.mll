(* repris et modifié directement d'aps :D *)

{
  open Parser
}
rule token = parse
    [' ' '\t' '\n' '\r']       { token lexbuf }
  | "lambda"                  { LAMBDA }
  | "ifZero"                  { IFZ}
  | "ifEmpty"                 { IFE }
  | "then"                    { THEN }
  | "else"                    { ELSE }
  | "let"                     { LET }
  | "in"                      { IN }
  | "fix"                     { PFIX }
  | "head"                      { HEAD }
  | "tail"                    { TAIL }
  | "ref"                     { REF }
  | ":="                      { ASSIGN }
  | "!"                       { DEREF }
  | "="                       { AFFECT }
  | ":"                       { COLON }
  | "["                       { LBRA }
  | "]"                       { RBRA }
  | "+"                       { ADD }
  | "-"                       { SUB }
  | "*"                       { MULT }
  | "("                       { LPAREN }
  | ")"                       { RPAREN }
  | ","                       { COMMA }
  | ";"                       { SEMICOLON }
  | "unit"                    { UNIT }
  | "[]"                      { EMPTYLIST }
  | ['0'-'9']+ as num         { INT (int_of_string num) }
  | ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* as ident { VAR ident }
  | eof                       { EOF }
