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

let test_typage_let_simple () =
  let env = [] in
  let term = Let ("id", Abs ("x", Var "x"), App (Var "id", Int 5)) in
  match inferer_type term env 100 with
  | Some t -> print_endline ("Type inféré pour Let simple : " ^ print_type t)
  | None -> print_endline "Erreur de typage pour Let simple"
;;

let test_typage_let_polymorphisme () =
  let term = Let ("id", Abs ("x", Var "x"), App (Var "id", Var "y")) in
  let env = [("y", TVar "B")] in
  match inferer_type term env 100 with
  | Some t -> print_endline ("Type inféré pour Let polymorphisme : " ^ print_type t)
  | None -> print_endline "Erreur de typage pour Let polymorphisme"
;;


let test_typage_pfix () =
  let env = [] in
  let term = Pfix (Abs ("f", Abs ("x", IfZero (Var "x", Int 1, Mul (Var "x", App (Var "f", Sub (Var "x", Int 1))))))) in
  match inferer_type term env 100 with
  | Some t -> print_endline ("Type inféré pour Pfix (factorielle) : " ^ print_type t)
  | None -> print_endline "Erreur de typage pour Pfix"
;;

let test_typage_liste_vide () =
  let env = [] in
  let term = List Vide in
  match inferer_type term env 100 with
  | Some t -> print_endline ("Type inféré pour une liste vide : " ^ print_type t)
  | None -> print_endline "Erreur de typage pour une liste vide"
;;


let test_typage_forall () =
  let env = [] in
  let term = Let ("id", Abs ("x", Var "x"), Var "id") in
  match inferer_type term env 100 with
  | Some t -> print_endline ("Type inféré pour la fonction identité polymorphe : " ^ print_type t)
  | None -> print_endline "Erreur de typage pour la fonction identité polymorphe"
;;

let test_typage_ifzero_simple () =
  let env = [] in
  let term = IfZero (Int 1, Int 1, Int 2) in
  match inferer_type term env 100 with
  | Some t -> print_endline ("Type inféré pour IfZero simple : " ^ print_type t)
  | None -> print_endline "Erreur de typage pour IfZero simple"
;;

let test_typage_ifzero_expression () =
  let env = [] in
  let term = IfZero (Add (Int 5, Int (-5)), Int 42, Add (Int 2, Int 3)) in
  match inferer_type term env 100 with
  | Some t -> print_endline ("Type inféré pour IfZero avec expressions : " ^ print_type t)
  | None -> print_endline "Erreur de typage pour IfZero avec expressions"
;;

let test_typage_unit () =
  let env = [] in
  let term = Unit in
  match inferer_type term env 100 with
  | Some t -> print_endline ("Type inféré pour Unit : " ^ print_type t)
  | None -> print_endline "Erreur de typage pour Unit"
;;

let test_typage_ref () =
  let env = [] in
  let term = Ref (Int 42) in
  match inferer_type term env 100 with
  | Some t -> print_endline ("Type inféré pour Ref 42 : " ^ print_type t)
  | None -> print_endline "Erreur de typage pour Ref 42"
;;

let test_typage_deref () =
  let env = [] in
  let term = Deref (Ref (Int 42)) in
  match inferer_type term env 100 with
  | Some t -> print_endline ("Type inféré pour Deref (Ref 42) : " ^ print_type t)
  | None -> print_endline "Erreur de typage pour Deref (Ref 42)"
;;

let test_typage_assign () =
  let env = [] in
  let term = Assign (Ref (Int 10), Int 20) in
  match inferer_type term env 100 with
  | Some t -> print_endline ("Type inféré pour Assign (Ref 10, 20) : " ^ print_type t)
  | None -> print_endline "Erreur de typage pour Assign (Ref 10, 20)"
;;

let test_typage_chaine_de_references () =
  let env = [] in
  let term = Deref (Deref (Ref (Ref (Int 5)))) in
  match inferer_type term env 100 with
  | Some t -> print_endline ("Type inféré pour chaîne de références : " ^ print_type t)
  | None -> print_endline "Erreur de typage pour chaîne de références"
;;

let test_typage_polymorphisme_faible () =
  let env = [] in
  let term = 
    Let ("l", Ref (List Vide), 
      Let ("t", Assign (Var "l", List (Cons (Abs ("x", Var "x"), Vide))), 
        Add (Head (Deref (Var "l")), Int 2)))
  in
  match inferer_type term env 100 with
  | Some t -> print_endline ("Erreur : le test de polymorphisme faible a réussi, type inféré : " ^ print_type t)
  | None -> print_endline "Test réussi : le typage a correctement rejeté le terme expansif"
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
  test_typage_forall();
  test_typage_ifzero_simple();
  test_typage_ifzero_expression();
  test_typage_unit ();
  test_typage_ref ();
  test_typage_deref ();
  test_typage_assign ();
  test_typage_chaine_de_references ();
  test_typage_polymorphisme_faible()