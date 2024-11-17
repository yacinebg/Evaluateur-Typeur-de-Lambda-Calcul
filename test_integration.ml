open Ast
open Lexer
open Parser
open Types

(* ces tests sont générés par chatGpt, c'est une grande aide j'aime beaucoup l'affichage*)
let parse_expression (input : string) : pterm =
  let lexbuf = Lexing.from_string input in
  try
    Parser.main Lexer.token lexbuf
  with
  | Parsing.Parse_error -> failwith "Erreur de parsing"
  | Failure msg -> failwith ("Erreur : " ^ msg)

let infer_type_expression (expr : pterm) (env : env) : ptype option =
  inferer_type expr env 100

let rec eval_expression (expr : pterm) (mem : memory) : pterm =
  match ltr_cbv_norm expr mem with
  | (result, _) -> result

let red text = "\027[31m" ^ text ^ "\027[0m"
let green text = "\027[32m" ^ text ^ "\027[0m"
let yellow text = "\027[33m" ^ text ^ "\027[0m"
let blue text = "\027[34m" ^ text ^ "\027[0m"
let separator = "----------------------------------------"

let test_expression (input : string) =
  print_endline separator;
  print_endline (blue ("Expression : " ^ input));
  print_endline separator;
  try
    let ast = parse_expression input in
    print_endline (yellow ("AST : " ^ Ast.print_term ast));

    match infer_type_expression ast [] with
    | Some ty ->
      print_endline (green ("Type inféré : " ^ Types.print_type ty));
      let result = eval_expression ast [] in
      print_endline (green ("Résultat de l'évaluation : " ^ Ast.print_term result))
    | None ->
      print_endline (red "Erreur d'inférence de type")
  with
  | Failure msg -> print_endline (red ("Erreur : " ^ msg))
  | _ -> print_endline (red "Erreur inconnue")

let () =
  let tests = [
    "42";
    "lambda x : x";
    "ifZero 0 then 1 else 2";
    "ifEmpty [] then 1 else 0";
    "let x := 10 in x + 1";
    "[]";
    "[1, 2, 3]";
    "ref 5";
    "! (ref 42)";
    "head [1, 2, 3]";
    "tail [1, 2, 3]";
  ] in
  List.iter test_expression tests;
  print_endline separator
