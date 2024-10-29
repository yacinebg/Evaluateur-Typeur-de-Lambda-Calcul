open Ast;;
open Types;;
let test_typage_variable () =
  let env = [("x", TVar "T1")] in
  let term = Var "x" in
  match inferer_type term env 100 with
  | Some t -> print_endline ("Type de Var 'x': " ^ print_type t)
  | None -> print_endline "Erreur de typage variable"
;;

let test_typage_abstraction_simple () =
  let env = [] in
  let term = Abs("x", Var "x") in
  match inferer_type term env 100 with
  | Some t -> print_endline ("Type inféré pour Abs 'x': " ^ print_type t)
  | None -> print_endline "Erreur de typage pour Abs 'x'"
;;

let test_typage_application_simple () =
  let env = [("f", Arr(TVar "T1", TVar "T2")); ("x", TVar "T1")] in
  let term = App(Var "f", Var "x") in
  match inferer_type term env 100 with
  | Some t -> print_endline ("Type inféré pour App 'f x': " ^ print_type t)
  | None -> print_endline "Erreur de typage pour App 'f x'"
;;


let () = 
test_typage_variable();
test_typage_abstraction_simple();
test_typage_application_simple ()