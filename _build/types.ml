open Unix
exception Timeout;;

type pterm = Var of string
  | App of pterm * pterm
  | Abs of string * pterm
;;


let rec print_term (t : pterm) : string = 
  match t with 
  | Var x -> x 
  | App (a, b) -> "("^ (print_term a)  ^" "^ (print_term b) ^")"
  | Abs (var, pterm) -> "(fun " ^ var ^" -> " ^(print_term pterm) ^")"
;;

let compteur_var : int ref = ref 0;;

let nouvelle_var () : string = compteur_var := !compteur_var + 1;
  "X"^(string_of_int !compteur_var);;



let rec substitution (x : string) (arg : pterm) (replace_in : pterm) : pterm = 
  match replace_in with
  | Var y -> if x = y then arg else replace_in 
  | App (a,b) -> App ((substitution x arg a ) , (substitution x arg b )) 
  | Abs (var, body) -> 
    if var = x then Abs(var, body) 
    else Abs(var, substitution x arg body) 
;;

let rec alphaconv (t : pterm) : pterm = match t with
  | Var x -> Var x
  | App (a,b) ->  App(alphaconv a, alphaconv b)
  | Abs (var, pterm) ->
    let new_var = nouvelle_var() in
    Abs (new_var, substitution var (Var new_var) (alphaconv pterm))
  ;;

let rec ltr_ctb_step (t : pterm) : pterm option =
  match t with
  | App (Abs (x, t1), n) -> 
    (match ltr_ctb_step n with
      | Some n' -> Some (substitution x n' t1)
      | None -> Some (substitution x n t1))
  | App (m, n) ->
    (match ltr_ctb_step m with
      | Some m' -> Some (App (m', n))
      | None -> 
        (match ltr_ctb_step n with
          | Some n' -> Some (App (m, n'))
          | None -> None))
  | _ -> None
;;

let rec ltr_cbv_norm (t : pterm) : pterm =
  match ltr_ctb_step t with
  | Some res -> ltr_cbv_norm res  
  | None -> t 
;;

(* on va mettre une limite de temps a la fonction puis soulever une exception *)
(* solution : https://stackoverflow.com/questions/70977947/how-to-efficiently-stop-a-function-after-a-certain-amount-of-time-in-ocaml *)
let timeout (duration: float) (f: unit -> 'a) : 'a =
  let start_time = Unix.gettimeofday () in
  let rec check_timeout () =
    let elapsed_time = Unix.gettimeofday () -. start_time in
    if elapsed_time > duration then raise Timeout
    else ()
  in
  let rec aux () =
    check_timeout ();
    f ()
  in
  aux ();;

(* je donne a la fonction une durée si elle est dépassé la fonction est arrétée *)
(* let rec ltr_cbv_norm_bis (t: pterm) (duration: float) : pterm option =
  try
    timeout duration (fun () -> 
      match ltr_ctb_step t with
      | Some res -> ltr_cbv_norm_bis res duration
      | None -> Some t 
    )
  with
  | Timeout -> None 
;;
*)
(* Examples *)
let identity = Abs("x", Var("x"));;
let s = Abs("x",Abs("y",Abs("z",App(App(Var ("x"),Var("z")),App(Var("y"),Var("z")))))) ;;
let k = Abs("x",Abs("y",Var("x")));;
let omega = App(Abs("x", App(Var "x", Var "x")), Abs("x", App(Var "x", Var "x")))
let delta = Abs("x", App(Var "x", Var "x"))

let skk = App (s,App(k,k));;
let sii = App(s,App(identity,identity))

let one = Abs("f",Abs("x",App(Var("f"),Var("x"))));;
let two = Abs("f",Abs("x",App(Var("f"),App(Var("f"),Var("x")))));;
let succ = Abs("n", Abs("f", Abs("x", App(Var "f", App(App(Var "n", Var "f"), Var "x")))))
let add = Abs("m", Abs("n", Abs("f", Abs("x", App(App(Var "m", Var "f"), App(App(Var "n", Var "f"), Var "x"))))))

let term_add = App(App(add, one), two);;
let result_add = ltr_cbv_norm term_add;;

let test () =
  let term_add = App(App(add, one), one) in
  let result_add = ltr_cbv_norm term_add in
  print_term result_add;;


let () = print_endline (test ());;
