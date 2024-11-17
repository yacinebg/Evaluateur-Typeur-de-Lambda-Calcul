type 'a liste = Vide | Cons of 'a * 'a liste
type pterm = Var of string
  | App of pterm * pterm
  | Abs of string * pterm
  
  (* partie 4 *)
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
  
  (* partie 5 *)
  | Ref of pterm               (* ref e *)
  | Deref of pterm             (* !e *)
  | Assign of pterm * pterm    (*e1 := e2 *)
  | Address of int 
  | Unit          
;;

type memory = (int * pterm) list
let mem_counter = ref 0
let new_mem () : int = 
  mem_counter := !mem_counter + 1;
  !mem_counter

(* Chercher une valeur dans la mémoire à partir de son adresse *)
let rec chercher_mem (addr : int) (mem : memory) : pterm option =
  match mem with
  | [] -> None
  | (a, v) :: rest -> if a = addr then Some v else chercher_mem addr rest

(* Mettre à jour la mémoire à une adresse donnée *)
let rec maj_mem (addr : int) (value : pterm) (mem : memory) : memory =
  match mem with
  | [] -> [(addr, value)]
  | (a, v) :: rest ->
      if a = addr then (addr, value) :: rest
      else (a, v) :: maj_mem addr value rest

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
  | Ref e -> "ref(" ^ print_term e ^ ")"
  | Deref e -> "!(" ^ print_term e ^ ")"
  | Assign (e1, e2) -> print_term e1 ^ " := " ^ print_term e2
  | Unit -> "()"
  | Address n -> "address " ^ (string_of_int n)
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
  | Ref e -> Ref (substitution x arg e)
  | Deref e -> Deref (substitution x arg e)
  | Assign (e1, e2) -> Assign (substitution x arg e1, substitution x arg e2)
  | Unit -> Unit
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
  (* partie 5 *)
  | Unit -> t 
  | Ref e -> Ref(alphaconv e)  
  | Deref e -> Deref(alphaconv e)  
  | Assign (e1,e2) -> Assign(alphaconv e1,alphaconv e2)  
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

let rec is_value t =
  match t with
  | Int _ | Unit | Abs _ | Address _ -> true
  | List l -> is_value_list l
  | _ -> false

and is_value_list l =
  match l with
  | Vide -> true
  | Cons (head, tail) -> is_value head && is_value_list tail


let rec ltr_ctb_step (t : pterm) (mem : memory) : (pterm * memory) option =
  match t with
  (* Réduction des applications *)
  | App (Abs (x, t1), n) ->
    (match ltr_ctb_step n mem with
     | Some (n', mem') -> Some (substitution x n' t1, mem')
     | None -> Some (substitution x n t1, mem))
  | App (m, n) ->
    (match ltr_ctb_step m mem with
     | Some (m', mem') -> Some (App (m', n), mem')
     | None ->
       (match ltr_ctb_step n mem with
        | Some (n', mem') -> Some (App (m, n'), mem')
        | None -> None))

  (* Réduction du Let *)
  | Let (var, e1, e2) ->
    (match ltr_ctb_step e1 mem with
     | Some (v, mem') when is_value v ->
       let subbed_e2 = substitution var v e2 in
       Some (subbed_e2, mem')
     | Some (e1', mem') -> Some (Let (var, e1', e2), mem')
     | None -> None)

  (* Réduction des opérations arithmétiques *)
  | Add (Int x, Int y) -> Some (Int (x + y), mem)
  | Add (t1, t2) ->
    (match ltr_ctb_step t1 mem with
     | Some (t1', mem') -> Some (Add (t1', t2), mem')
     | None ->
       (match ltr_ctb_step t2 mem with
        | Some (t2', mem') -> Some (Add (t1, t2'), mem')
        | None -> None))

  | Sub (Int x, Int y) -> Some (Int (x - y), mem)
  | Sub (t1, t2) ->
    (match ltr_ctb_step t1 mem with
     | Some (t1', mem') -> Some (Sub (t1', t2), mem')
     | None ->
       (match ltr_ctb_step t2 mem with
        | Some (t2', mem') -> Some (Sub (t1, t2'), mem')
        | None -> None))

  | Mul (Int x, Int y) -> Some (Int (x * y), mem)
  | Mul (t1, t2) ->
    (match ltr_ctb_step t1 mem with
     | Some (t1', mem') -> Some (Mul (t1', t2), mem')
     | None ->
       (match ltr_ctb_step t2 mem with
        | Some (t2', mem') -> Some (Mul (t1, t2'), mem')
        | None -> None))

  (* Réduction des opérations sur les listes *)
  | Head (List (Cons (x, _))) -> Some (x, mem)
  | Head (List Vide) -> failwith "Head of empty list"
  | Head lst ->
    (match ltr_ctb_step lst mem with
     | Some (List (Cons (x, _)), mem') -> Some (x, mem')
     | _ -> None)

  | Tail (List (Cons (_, xs))) -> Some (List xs, mem)
  | Tail (List Vide) -> failwith "Tail of empty list"
  | Tail lst ->
    (match ltr_ctb_step lst mem with
     | Some (List (Cons (_, xs)), mem') -> Some (List xs, mem')
     | _ -> None)

  (* Réduction des expressions conditionnelles *)
  | IfZero (Int 0, t1, _) -> Some (t1, mem)
  | IfZero (Int _, _, t2) -> Some (t2, mem)
  | IfZero (cond, t1, t2) ->
    (match ltr_ctb_step cond mem with
     | Some (Int 0, mem') -> Some (t1, mem')
     | Some (Int _, mem') -> Some (t2, mem')
     | _ -> None)

  | IfEmpty (List Vide, t1, _) -> Some (t1, mem)
  | IfEmpty (List (Cons _), _, t2) -> Some (t2, mem)
  | IfEmpty (cond, t1, t2) ->
    (match ltr_ctb_step cond mem with
     | Some (List Vide, mem') -> Some (t1, mem')
     | Some (List (Cons _), mem') -> Some (t2, mem')
     | _ -> None)

  (* Réduction de Pfix *)
  | Pfix t ->
    (match t with
     | Abs (f, body) ->
       let t' = substitution f (Pfix t) body in
       Some (t', mem)
     | _ -> failwith "Pfix doit être appliqué à une abstraction")

  | Ref v when is_value v ->
    let addr = new_mem () in
    Some (Address addr, (addr, v) :: mem)
  | Ref t ->
      (match ltr_ctb_step t mem with
       | Some (v, mem') when is_value v -> 
           let addr = new_mem () in
           Some (Address addr, (addr, v) :: mem')
       | Some (t', mem') -> Some (Ref t', mem')
       | None -> None)
  | Assign (Address a, v) when is_value v ->
      Some (Unit, (a, v) :: List.remove_assoc a mem)
  | Assign (t1, t2) ->
      (match ltr_ctb_step t1 mem with
       | Some (t1', mem') -> Some (Assign (t1', t2), mem')
       | None when is_value t1 ->
           (match ltr_ctb_step t2 mem with
            | Some (t2', mem') -> Some (Assign (t1, t2'), mem')
            | None -> None)
       | None -> None)
  | Deref (Address a) -> 
      (try Some (List.assoc a mem, mem)
       with Not_found -> failwith ("Invalid address: " ^ string_of_int a))
  | Deref t ->
      (match ltr_ctb_step t mem with
       | Some (t', mem') -> Some (Deref t', mem')
       | None -> None)
     
  |_ -> None
;;


let rec ltr_cbv_norm (t : pterm) (mem : memory) : (pterm * memory) =
  match ltr_ctb_step t mem with
  | Some (res, mem') -> ltr_cbv_norm res mem'
  | None -> (t, mem)
;;

let rec ltr_cbv_norm_with_limit (t : pterm) (mem : memory) (limit : int) : (pterm * memory) option =
  if limit = 0 then None
  else
    match ltr_ctb_step t mem with
    | Some (res, mem') -> ltr_cbv_norm_with_limit res mem' (limit - 1)
    | None -> Some (t, mem)
;;

