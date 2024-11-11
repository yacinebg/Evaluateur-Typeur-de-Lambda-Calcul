type 'a liste = Vide | Cons of 'a * 'a liste
type pterm = Var of string
  | App of pterm * pterm
  | Abs of string * pterm
  | Let of string * pterm * pterm
  | Pfix of pterm
  | IfZero of pterm * pterm * pterm
  | IfEmpty of pterm * pterm * pterm
  | Int of int
  | Add of pterm * pterm
  | Sub of pterm * pterm
  | Mul of pterm * pterm
  | List of pterm liste
  | Head of pterm
  | Tail of pterm
  | Cons of pterm * pterm 
;;

   
(* print des termes  *)
let rec print_term (t : pterm) : string = 
  match t with 
  | Var x -> x
  | App (a, b) -> "(" ^ (print_term a) ^ " " ^ (print_term b) ^ ")"
  | Abs (var, pterm) -> "(fun " ^ var ^ " -> " ^ (print_term pterm) ^ ")"
  | Int n -> string_of_int n  (* Assurez-vous que ce cas gère Int correctement *)
  | Add (a, b) -> "(" ^ (print_term a) ^ " + " ^ (print_term b) ^ ")"
  | Sub (a, b) -> "(" ^ (print_term a) ^ " - " ^ (print_term b) ^ ")"
  | Mul (a, b) -> "(" ^ (print_term a) ^ " * " ^ (print_term b) ^ ")"
  | IfZero (cond, t1, t2) -> "if zero " ^ (print_term cond) ^ " then " ^ (print_term t1) ^ " else " ^ (print_term t2)
  | Cons (head, tail) -> "(" ^ (print_term head) ^ " :: " ^ (print_term tail) ^ ")"
  | Head lst -> (print_term lst)  (* Vérifiez bien ce cas si l'évaluation est complète *)
  | Tail lst -> (print_term lst)
  | IfEmpty (cond, t1, t2) -> 
      "if empty " ^ (print_term cond) ^ " then " ^ (print_term t1) ^ " else " ^ (print_term t2)
  | Pfix t -> "fix " ^ (print_term t)
  | Let (x, e1, e2) -> "let " ^ x ^ " = " ^ (print_term e1) ^ " in " ^ (print_term e2)
  | List elements -> 
    let rec aux lst =
      match lst with
      | Vide -> ""
      | Cons (head, Vide) -> print_term head
      | Cons (head, tail) -> print_term head ^ "; " ^ aux tail
    in "[" ^ aux elements ^ "]"
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
  | Add (a, b) -> Add (substitution x arg a, substitution x arg b)
  | Sub (a, b) -> Sub (substitution x arg a, substitution x arg b)
  | Mul (a, b) -> Mul (substitution x arg a, substitution x arg b)
  | Let (var, e1, e2) -> 
    if var = x then Let(var, substitution x arg e1, e2)
    else 
      Let(var, substitution x arg e1, substitution x arg e2)
  | Pfix t -> Pfix (substitution x arg t) 
  | IfZero (cond, t1, t2) -> IfZero (substitution x arg cond, substitution x arg t1, substitution x arg t2)
  | IfEmpty (cond, t1, t2) -> IfEmpty (substitution x arg cond, substitution x arg t1, substitution x arg t2)
  | List elements -> 
    let rec substitute_in_list ls =
      match ls with
      | Vide -> Vide
      | Cons (head, tail) -> Cons (substitution x arg head, substitute_in_list tail)
    in
    List (substitute_in_list elements)

  | Head lst -> Head (substitution x arg lst)
  | Tail lst -> Tail (substitution x arg lst)
  | Cons (head, tail) -> Cons (substitution x arg head, substitution x arg tail)
  | _ -> replace_in
;;

let rec substitute_in_list (lst : 'a liste) (x : string) (arg : pterm) : 'a liste =
  match lst with
  | Vide -> Vide
  | Cons (head, tail) -> 
      Cons (substitution x arg head, substitute_in_list tail x arg)
  ;;

let rec alphaconv (t : pterm) : pterm = match t with
  | Var x -> Var x
  | App (a,b) ->  App(alphaconv a, alphaconv b)
  | Abs (var, pterm) ->
    let new_var = nouvelle_var() in
    Abs (new_var, substitution var (Var new_var) (alphaconv pterm))
  | Let (var, e1, e2) -> 
    let new_var = nouvelle_var() in
    Let (new_var, alphaconv e1, substitution var (Var new_var) (alphaconv e2))
  | Pfix t -> Pfix (alphaconv t)
  | IfZero (cond, t1, t2) -> 
    IfZero (alphaconv cond, alphaconv t1, alphaconv t2)
  | IfEmpty (cond, t1, t2) -> 
    IfEmpty (alphaconv cond, alphaconv t1, alphaconv t2)
  | Add (a, b) -> Add (alphaconv a, alphaconv b)
  | Sub (a, b) -> Sub (alphaconv a, alphaconv b)
  | Mul (a, b) -> Mul (alphaconv a, alphaconv b)
  | List elements -> List(alphaconv_list elements)
  | Head lst -> Head (alphaconv lst)
  | Tail lst -> Tail (alphaconv lst)
  | Cons (head, tail) -> Cons (alphaconv head, alphaconv tail)
  | _ -> t
  
  and alphaconv_list (lst : pterm liste ): pterm liste = 
  let rec aux (lst : pterm liste ): pterm liste= (match lst with
    | Vide -> Vide 
    | Cons (hd,tail) -> 
        let h = alphaconv hd in 
        let tl = aux tail in 
        Cons(h,tl) 
  ) in 
  aux lst
;; 


let rec ltr_ctb_step (t : pterm) : pterm option =
  match t with
  (* | Abs (x, body) -> (match ltr_ctb_step body with
    | Some new_body -> Some (Abs (x, new_body))
    | None -> None) *)
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
  |Let (var,e1,e2) ->(match (ltr_ctb_step e1) with
    | Some v -> let subbed_e2 = substitution var v e2 in  Some subbed_e2
    | None ->  let subbed_e2 = substitution var e1 e2 in  Some subbed_e2)
  | Pfix (t) -> (match t with
      | Abs(phi,m) ->let subbed = substitution phi (Pfix(t)) m in Some subbed 
      | _ -> None )
  | IfZero (Int(0),cons,alt) -> Some(cons) 
  | IfZero (Int(n),cons,alt) -> Some(alt) 
  | IfZero (cond,cons,alt) -> (match ltr_ctb_step cond with 
    | Some cond' -> Some (IfZero (cond',cons,alt)  ) 
    | None -> failwith "If condition should be an integer " 
    )
  | IfEmpty (lst, t1, t2) -> 
    (match lst with
      | List(Vide) -> Some t1
      | List(Cons(z,y)) -> Some t2
      | _ -> None) 
  
  | Add(t1,t2) -> (match ltr_ctb_step t1 with
    | Some(t1')-> Some(Add(t1',t2))
    | None -> (match ltr_ctb_step t2 with 
      | Some(t2') -> Some(Add(t1,t2'))
      | None -> (match(t1,t2) with 
        |(Int(x),Int(y)) -> Some(Int(x+y))
        |_ -> None)))
  | Sub(t1,t2) -> (match ltr_ctb_step t1 with
    | Some(t1')-> Some(Sub(t1',t2))
    | None -> (match ltr_ctb_step t2 with 
      | Some(t2') -> Some(Sub(t1,t2'))
      | None -> (match(t1,t2) with 
        |(Int(x),Int(y)) -> Some(Int(x-y))
        |_ -> None)))

  | Mul(t1,t2) -> (match ltr_ctb_step t1 with
    | Some(t1')-> Some(Mul(t1',t2))
    | None -> (match ltr_ctb_step t2 with 
      | Some(t2') -> Some(Mul(t1,t2'))
      | None -> (match(t1,t2) with 
        |(Int(x),Int(y)) -> Some(Int(x*y))
        |_ -> None)))
  
  | Head (List (Cons (x, _))) -> Some x
  | Head (List Vide) -> None
  | Head _ -> None

  | Tail (List (Cons (_, xs))) -> Some (List xs)
  | Tail (List Vide) -> failwith "liste vide"
  | Tail lst -> 
    (match ltr_ctb_step lst with 
     | Some List (Cons (_, xs)) -> Some (List xs)
     | Some List Vide -> None
     | _ -> None)

  |_ -> None
;;

let rec ltr_cbv_norm (t : pterm) : pterm =
  match ltr_ctb_step t with
  | Some res -> ltr_cbv_norm res  
  | None -> t 
;;



let rec ltr_cbv_norm_with_limit (t : pterm) (limit : int) : pterm option =
  if limit = 0 then None
  else
    match ltr_ctb_step t with
    | Some res -> ltr_cbv_norm_with_limit res (limit - 1)
    | None -> Some t
;;
