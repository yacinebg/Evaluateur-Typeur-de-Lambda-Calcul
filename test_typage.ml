open Ast;;
open Types;;

let test_typage_entier () =
  let env = [] in
  let term = Int 42 in
  match inferer_type term env 100 with
  | Some t -> print_endline ("Type inféré pour Int 42 : " ^ print_type t)
  | None -> print_endline "Erreur de typage pour Int 42"
;;

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
  let env = [("f", Arr(TVar "B1", TVar "B2")); ("x", TVar "B1")] in
  let term = App(Var "f", Var "x") in
  match inferer_type term env 100 with
  | Some t -> print_endline ("Type inféré pour App 'f x': " ^ print_type t)
  | None -> print_endline "Erreur de typage pour App 'f x'"
;;


(* Test pour le typage des listes *)
let test_typage_cons () =
  let env = [] in
  let term = Cons (Int 1, List (Cons (Int 2, Vide))) in
  match inferer_type term env 100 with
  | Some t -> print_endline ("Type inféré pour Cons : " ^ print_type t)
  | None -> print_endline "Erreur de typage pour Cons"
;;

let test_typage_head () =
  let env = [] in
  let term = Head (List (Cons (Int 1, Vide))) in
  match inferer_type term env 100 with
  | Some t -> print_endline ("Type inféré pour Head : " ^ print_type t)
  | None -> print_endline "Erreur de typage pour Head"
;;

let test_typage_tail () =
  let env = [] in
  let term = Tail (List (Cons (Int 1, Vide))) in
  match inferer_type term env 100 with
  | Some t -> print_endline ("Type inféré pour Tail : " ^ print_type t)
  | None -> print_endline "Erreur de typage pour Tail"
;;

(* Test pour le typage du let-polymorphisme *)
let test_typage_let_simple () =
  let env = [] in
  let term = Let ("id", Abs ("x", Var "x"), App (Var "id", Int 5)) in
  match inferer_type term env 100 with
  | Some t -> print_endline ("Type inféré pour Let simple : " ^ print_type t)
  | None -> print_endline "Erreur de typage pour Let simple"
;;

(* Test pour le typage d'une expression Let avec polymorphisme *)
let test_typage_let_polymorphisme () =
  let env = [] in
  let term = Let ("id", Abs ("x", Var "x"), App (Var "id", Var "y")) in
  let env_with_y = [("y", TVar "B")] in
  match inferer_type term env_with_y 100 with
  | Some t -> print_endline ("Type inféré pour Let polymorphisme : " ^ print_type t)
  | None -> print_endline "Erreur de typage pour Let polymorphisme"
;;


(* Test pour le typage du point fixe (Pfix) *)
let test_typage_pfix () =
  let env = [] in
  let term = Pfix (Abs ("f", Abs ("x", IfZero (Var "x", Int 1, Mul (Var "x", App (Var "f", Sub (Var "x", Int 1))))))) in
  match inferer_type term env 100 with
  | Some t -> print_endline ("Type inféré pour Pfix (factorielle) : " ^ print_type t)
  | None -> print_endline "Erreur de typage pour Pfix"
;;

(* Test pour le typage d'une liste vide *)
let test_typage_liste_vide () =
  let env = [] in
  let term = List Vide in
  match inferer_type term env 100 with
  | Some t -> print_endline ("Type inféré pour une liste vide : " ^ print_type t)
  | None -> print_endline "Erreur de typage pour une liste vide"
;;

(* Test pour le typage d'une fonction généralisée *)

let test_generaliser () =
  let env1 = [("x", TVar "A")] in
  let ty1 = Arr (TVar "A", TVar "B") in
  let ty_gen = generaliser ty1 env1 in
  print_endline ("Type avant généralisation : " ^ print_type ty1);
  print_endline ("Type après généralisation : " ^ print_type ty_gen)
;;



let () = 
test_typage_variable ();
  test_typage_abstraction_simple ();
  test_typage_application_simple ();
  test_typage_cons ();
  test_typage_head ();
  test_typage_tail ();
  test_typage_let_simple ();
  test_typage_let_polymorphisme ();
  test_typage_pfix ();
  test_typage_liste_vide ();