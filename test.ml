open Ast
(* Examples *)
let identity = Abs("x", Var("x"));;
let k = Abs("x", Abs("y", Var "x"))
let s = Abs("x", Abs("y", Abs("z", App(App(Var "x", Var "z"), App(Var "y", Var "z")))))
let skk = App(App(s,k),k)
let omega = App(Abs("x", App(Var "x", Var "x")), Abs("x", App(Var "x", Var "x")))
let delta = Abs("x", App(Var "x", Var "x"))

let sii = App(App(s,identity),identity)

let one = Abs("f",Abs("x",App(Var("f"),Var("x"))));;
let two = Abs("f",Abs("x",App(Var("f"),App(Var("f"),Var("x")))));;
let succ = Abs("n", Abs("f", Abs("x", App(Var "f", App(App(Var "n", Var "f"), Var "x")))))
let add = Abs("m", Abs("n", Abs("f", Abs("x", App(App(Var "m", Var "f"), App(App(Var "n", Var "f"), Var "x"))))))

let term_add = App(App(add, one), two);;
let result_add = ltr_cbv_norm term_add;;

let test  =
  let term_add = skk in
  let result_add = ltr_cbv_norm term_add in
  print_term result_add;;

let () = print_endline (test)