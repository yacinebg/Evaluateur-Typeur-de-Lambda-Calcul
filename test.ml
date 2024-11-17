open Ast

(* Examples *)
let identity = Abs("x", Var("x"));;
let k = Abs("x", Abs("y", Var "x"))
let s = Abs("x", Abs("y", Abs("z", App(App(Var "x", Var "z"), App(Var "y", Var "z")))))
let skk = App(App(s, k), k)
let omega = App(Abs("x", App(Var "x", Var "x")), Abs("x", App(Var "x", Var "x")))
let delta = Abs("x", App(Var "x", Var "x"))

let sii = App(App(s, identity), identity)

let one = Abs("f", Abs("x", App(Var("f"), Var("x"))));;
let two = Abs("f", Abs("x", App(Var("f"), App(Var("f"), Var("x")))));;
let succ = Abs("n", Abs("f", Abs("x", App(Var "f", App(App(Var "n", Var "f"), Var "x")))))
let add = Abs("m", Abs("n", Abs("f", Abs("x", App(App(Var "m", Var "f"), App(App(Var "n", Var "f"), Var "x"))))))

let term_add = App(App(add, one), two)

(* Initialisation de la mémoire *)
let initial_memory = []

let result_add, _ = ltr_cbv_norm term_add initial_memory;;

let test =
  let term_add = skk in
  let result_add, _ = ltr_cbv_norm term_add initial_memory in
  print_term result_add;;

(* Tests pour la partie 5 *)
let resultats_tests = [
  ("Addition : 2+2", Add(Int(2), Int(2)), Int(4));
  ("Mult : 6*2", Mul(Int(6), Int(2)), Int(12));
  ("Sub : 2-2", Sub(Int(2), Int(2)), Int(0));
  ("Addition : (2+2) + 2", Add(Add(Int(2), Int(2)), Int(2)), Int(6));
  ("Sub et add : 2-1+2", Add(Sub(Int(2), Int(1)), Int(2)), Int(3));
]

let examples_list = [
  ("head ([1])", (Head (List(Cons(Int(1), Vide)))), Int 1);
  ("Tail [1]", (Tail (List(Cons(Int(1), Vide)))), List Vide);
  ("Head [1,2,3]", (Head (List(Cons(Int(1), Cons(Int(2), Cons(Int(3), Vide)))))), Int 1);
  ("Tail [1,2,3]", (Tail (List(Cons(Int(1), Cons(Int(2), Cons(Int(3), Vide)))))), List(Cons(Int(2), Cons(Int(3), Vide))))
]

let examples_if = [
  ("IfZero (1+2) 1 2", (IfZero(Add(Int(1), Int(2)), Int(1), Int(2))), Int 2);
  ("IfEmpty ([]) 1 2", (IfEmpty(List(Vide), Int(1), Int(2))), Int 1);
]

let factorial =
  Pfix (Abs ("f", Abs ("n",
    IfZero (Var "n",
      Int 1,
      Mul (Var "n", App (Var "f", Sub (Var "n", Int 1)))
    )
  )))
;;

let tests_factorial = [
  ("Factorielle de 0", App (factorial, Int 0), Int 1);
  ("Factorielle de 1", App (factorial, Int 1), Int 1);
  ("Factorielle de 3", App (factorial, Int 3), Int 6);
  ("Factorielle de 5", App (factorial, Int 5), Int 120);
]

(* Tests pour Ref, Deref et Assign *)
let tests_memory = [
  ("Création de référence", Ref (Int 42), Address 1);

  ("Déréférencement de référence",
   Let ("x", Ref (Int 10), Deref (Var "x")),
   Int 10);

  ("Assignation de valeur à une référence",
   Let ("x", Ref (Int 10), Assign (Var "x", Int 20)),
   Unit);

  ("Déréférencement après assignation",
   Let ("x", Ref (Int 10),
     Let ("_", Assign (Var "x", Int 20), Deref (Var "x"))),
   Int 20);

  ("Chaîne de références",
   Let ("x", Ref (Int 5),
     Let ("y", Ref (Var "x"),
       Deref (Deref (Var "y")))),
   Int 5);
]

(* Fonction pour afficher les résultats des tests *)
let afficher_tests tests =
  List.iter (fun (description, term, expected) ->
    let result, _ = ltr_cbv_norm term initial_memory in
    if result = expected then
      Printf.printf "%s : %s = %s (Test réussi)\n" description (print_term term) (print_term result)
    else
      Printf.printf "%s : %s = %s, attendu %s (Test échoué)\n" description (print_term term) (print_term result) (print_term expected)
  ) tests

(* Exécution des tests *)
let () =
  afficher_tests resultats_tests;
  afficher_tests examples_list;
  afficher_tests examples_if;
  afficher_tests tests_factorial;
  afficher_tests tests_memory
