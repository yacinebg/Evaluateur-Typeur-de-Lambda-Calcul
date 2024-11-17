open Ast

(* Types simples*)
type ptype = 
  | TVar of string
  | Arr of ptype * ptype
  | N
  | TList of ptype   
  | Forall of string list * ptype 
  | TUnit
  | TRef of ptype
;;


(* print des types *)
let rec print_type (t : ptype) : string = 
  match t with
  | TVar x -> x
  | Arr (t1, t2) -> "(" ^ (print_type t1) ^ " -> " ^ (print_type t2) ^ ")"
  | N -> "N"
  | TList t -> "[" ^ (print_type t) ^ "]"
  | Forall (vars, t) ->
    let vars_str = String.concat ", " vars in
    "∀" ^ vars_str ^ ". " ^ (print_type t)
  | TRef t -> "ref ("^ (print_type t) ^")"
  | TUnit  -> "unit "
;;



let compteur_var_t : int ref = ref 0
let nouvelle_var_t () : string = compteur_var_t := ! compteur_var_t + 1;"T"^( string_of_int ! compteur_var_t );;

type equa = (ptype * ptype) list

type env = (string * ptype) list

let rec cherche_type (v : string) (e : env) : ptype =
  match e with
  | [] -> failwith("la variable n'est pas dans l'env")
  | (x,xs)::rest -> if x = v then xs else cherche_type v rest
;; 


let rec est_non_expansif (t : pterm) : bool =
  match t with
  | Var _ -> true
  | Int _ -> true
  | Unit -> true
  | Abs _ -> true
  | List Vide -> true
  | Add (t1, t2) | Sub (t1, t2) | Mul (t1, t2) -> 
      est_non_expansif t1 && est_non_expansif t2
  | IfZero (t1, t2, t3) | IfEmpty (t1, t2, t3) -> 
      est_non_expansif t1 && est_non_expansif t2 && est_non_expansif t3
  | Cons (t1, t2) ->
      est_non_expansif t1 && est_non_expansif t2
  | List lst -> 
    let rec est_non_expansif_liste l =
      match l with
      | Vide -> true
      | Cons (hd, tl) -> est_non_expansif hd && est_non_expansif_liste tl
    in
    est_non_expansif_liste lst
  | Let (_, e1, _) -> 
      est_non_expansif e1
  (* Cas expansive *)
  | App _ -> false
  | Ref _ -> false
  | Deref _ -> false
  | Assign _ -> false
  | Pfix _ -> false
  | Head _ -> false
  | Tail _ -> false


(* Récupérer les variables libres qui ne sont pas dans l'environnement *)
let rec generaliser (t : ptype) (env : env) (term : pterm) : ptype =
    let rec collect_vars ty acc =
      match ty with
      | TVar x -> if List.mem x acc then acc else x :: acc
      | Arr (t1, t2) -> collect_vars t1 (collect_vars t2 acc)
      | TList t -> collect_vars t acc
      | TRef t -> collect_vars t acc
      | Forall (vars, t) ->
          let acc_filtered = List.filter (fun x -> not (List.mem x vars)) acc in
          collect_vars t acc_filtered
      | N | TUnit -> acc
    in
    let vars_env = List.fold_left (fun acc (_, ty) -> collect_vars ty acc) [] env in
    let vars_libres = collect_vars t [] in
    let vars_a_generaliser = List.filter (fun x -> not (List.mem x vars_env)) vars_libres in
    if vars_a_generaliser = [] then t
    else Forall (vars_a_generaliser, t)
;;

let rec generaliser_faible (ty : ptype) (env : env) (term : pterm) : ptype =
  if est_non_expansif term then
    generaliser ty env term
  else
    ty
;;


let rec genere_equa (te : pterm) (ty : ptype) (e : env) : equa =
  match te with 
  | Var v -> 
      let tv = (cherche_type v e) in 
      [(tv, ty)]
  | Abs (x, t) -> 
      let ta = TVar (nouvelle_var_t()) in
      let tr = TVar (nouvelle_var_t()) in
      let env' = (x, ta) :: e in
      let eq = genere_equa t tr env' in
      (ty, Arr (ta, tr)) :: eq
  | App (t1, t2) ->
      let ta = TVar (nouvelle_var_t()) in
      let eq_gauche = genere_equa t1 (Arr(ta, ty)) e in
      let eq_droite = genere_equa t2 ta e in
      eq_gauche @ eq_droite
  (* Question 4.2 *)
  | Int _ -> [(ty, N)]
  | Add (x, y) | Sub (x, y) | Mul (x, y) ->
    let eq_gauche = genere_equa x N e in
    let eq_droite = genere_equa y N e in
    [(ty, N)] @ eq_gauche @ eq_droite
  
  | IfZero (cond, t1, t2) -> 
    let tc = genere_equa cond N e in
    let eq_t1 = genere_equa t1 ty e in
    let eq_t2 = genere_equa t2 ty e in
    tc @ eq_t1 @ eq_t2
  
  | IfEmpty (cond, t1, t2) ->
    let ta = TVar (nouvelle_var_t()) in
    let eq_cond = genere_equa cond (TList ta) e in
    let eq_t1 = genere_equa t1 ty e in
    let eq_t2 = genere_equa t2 ty e in
    eq_cond @ eq_t1 @ eq_t2
  
  | Let (x, e1, e2) ->
    let t0 = TVar (nouvelle_var_t()) in
    let inferred_type = inferer_type e1 e 100 in
    let t0' = match inferred_type with
      | Some ty -> ty
      | None -> failwith "Erreur lors de l'inférence du type de e1"
    in
    let t0_gen = generaliser_faible t0' e e1 in
    let env' = (x, t0_gen) :: e in
    genere_equa e2 ty env'
  
  | Pfix t ->
    (match t with
      | Abs (x, t_body) ->
          let type_T = TVar (nouvelle_var_t ()) in
          let type_U = TVar (nouvelle_var_t ()) in
          let env' = (x, Arr (type_T, type_U)) :: e in
          let eqs_m = genere_equa t_body (Arr (type_T, type_U)) env' in
          (ty, Arr (type_T, type_U)) :: eqs_m
      | _ -> failwith "Pfix doit être appliqué à une abstraction")
  

  | Head lst ->
    let ta = TVar (nouvelle_var_t()) in
    let eq_lst = genere_equa lst (TList ta) e in
    (ty, ta) :: eq_lst
 
  | Tail lst ->
    let ta = TVar (nouvelle_var_t()) in
    let eq_lst = genere_equa lst (TList ta) e in
    (ty, TList ta) :: eq_lst

  | Cons (head, tail) ->
      let ta = TVar (nouvelle_var_t()) in
      let eq_head = genere_equa head ta e in
      let eq_tail = genere_equa tail (TList ta) e in
      (ty, TList ta) :: eq_head @ eq_tail

  
  | List xs ->
    let ta = TVar (nouvelle_var_t ()) in
    (match xs with
      | Vide ->[(ty, TList ta)]
      | Cons (hd, tl) -> 
          let eq_head = genere_equa hd ta e in
          let eq_tail = genere_equa (List tl) (TList ta) e in
          (ty, TList ta) :: eq_head @ eq_tail)

  | Ref t ->
    let ta = TVar (nouvelle_var_t()) in
    let eq_t = genere_equa t ta e in
    (ty, TRef ta) :: eq_t
      
  | Deref t ->
    let ta = TVar (nouvelle_var_t()) in
    let eq_t = genere_equa t (TRef ta) e in
    (ty, ta) :: eq_t
  
  | Assign (t1, t2) ->
    let ta = TVar (nouvelle_var_t()) in
    let eq1 = genere_equa t1 (TRef ta) e in
    let eq2 = genere_equa t2 ta e in
    (ty, TUnit) :: eq1 @ eq2
  
  | Unit -> [(ty,TUnit)]

(* fonction qui verifie si une variable est dans un type *)
and occur_check (v:string) (ty:ptype) : bool = 
  match ty with
  | TVar x -> x == v 
  | Arr (t1,t2) -> occur_check v t1 || occur_check v t2
  | TList t -> occur_check v t
  | Forall (vars, t) ->
      if List.mem v vars then false
      else occur_check v t
  | N -> false
  | _ -> false


and substitue (v:string) (replace_with : ptype) (t:ptype) : ptype=
  match t with 
  |TVar x -> if x = v then replace_with else t 
  | N-> N
  |Arr(t1,t2) -> Arr((substitue v replace_with t1),(substitue v replace_with t2))
  | TList t' -> TList (substitue v replace_with t')
  | Forall (vars, t') ->
      if List.mem v vars then t
      else Forall (vars, substitue v replace_with t')
  | TRef t' -> TRef (substitue v replace_with t')
  | TUnit -> TUnit

  | _ -> failwith "dz"

and substitute_in_equations (v : string) (replacement : ptype) (equations : equa) : equa =
  match equations with
  | [] -> []
  | (t1, t2) :: rest ->
      let new_t1 = substitue v replacement t1 in
      let new_t2 = substitue v replacement t2 in
      (new_t1, new_t2) :: substitute_in_equations v replacement rest


and barendregtisation (vars : string list) (t : ptype) : ptype =
  let substitutions = List.map (fun v -> (v, TVar (nouvelle_var_t ()))) vars in
  List.fold_left (fun acc (v, tv) -> substitue v tv acc) t substitutions
      
(* fonction qui implémente l'algorithme d'unification des fonctions *)
and unification (equations : equa) (subs : equa) : equa =
  match equations with
  | [] -> subs
  | (t1, t2) :: rest -> 
      if t1 = t2 then unification rest subs
      else 
        match (t1, t2) with
        | N, N -> unification rest subs
        | TVar x, _ -> 
            if not (occur_check x t2) then 
              let new_subs = (TVar x, t2) :: subs in 
              let updated_rest = substitute_in_equations x t2 rest in
              unification updated_rest new_subs
            else failwith "échec (occur check)"
        | _, TVar x -> 
            if not (occur_check x t1) then 
              let new_subs = (TVar x, t1) :: subs in 
              let updated_rest = substitute_in_equations x t1 rest in
              unification updated_rest new_subs
            else failwith "échec (occur check)"
        | Arr (tga, tgr), Arr (tda, tdr) -> 
            unification ((tga, tda) :: (tgr, tdr) :: rest) subs
        
        | Forall (vars, t), _ ->
          let t' = barendregtisation vars t in
          unification ((t', t2) :: rest) subs      

        | _, Forall (vars, t) ->
          let t' = barendregtisation vars t in
          unification ((t1, t') :: rest) subs

        | TList t1', TList t2' -> 
          unification ((t1', t2') :: rest) subs
        
        | TRef t1', TRef t2' ->
          unification ((t1', t2') :: rest) subs
        
        | TUnit, TUnit -> 
          unification rest subs

        | _ -> failwith "échec (types incompatibles)"


and appliquer_substitution (equations : equa) (t : ptype) : ptype =
  match t with
  | TVar v -> (
      try
        let t' = List.assoc (TVar v) equations in
        appliquer_substitution equations t'
      with Not_found -> TVar v
    )
  | Arr (t1, t2) -> Arr (appliquer_substitution equations t1, appliquer_substitution equations t2)
  | TList t' -> TList (appliquer_substitution equations t')
  | Forall (vars, t') ->
      let filtered_equations = List.filter (fun (TVar x, _) -> not (List.mem x vars)) equations in
      Forall (vars, appliquer_substitution filtered_equations t')
  | _ -> t


(* fonction qui utilise l'unification pour résoudre les equations *)
and resoudre_equations equations limit = 
  try Some (unification equations [])
  with _ -> None

(* cette fonction est utile pour rendre le bon type durant le typage, 
ex: apres la resolution des equation T1 = N, le type retourné etait T1 or nous on veut N 
 *)
and trouver_variable_originale (equations : equa) (t : ptype) : ptype =
  match t with
  | TVar _ ->
      (try
          List.find (fun (_, t2) -> t2 = t) equations |> fst
        with Not_found -> t)
  | _ -> t
  
and inferer_type (term : pterm) (env : env) (limit : int) : ptype option =
  let t = TVar (nouvelle_var_t()) in
  let equations = genere_equa term t env in
  (* print_endline "=== Équations générées ===";
  print_equations equations; *)
  match resoudre_equations equations limit with
  | None -> print_endline "Échec de l'unification des équations"; None
  | Some eqs -> 
    (* print_endline "=== Substitutions appliquées ===";
    print_substitutions eqs; *)
    let final_type = appliquer_substitution eqs t in
    let resolved_type = appliquer_substitution eqs final_type in
    let fully_resolved_type = appliquer_substitution eqs resolved_type in
    let variable_originale = trouver_variable_originale eqs fully_resolved_type in
    Some variable_originale


and  print_substitutions (equations : equa) =
  List.iter (fun (t1, t2) -> print_endline (print_type t1 ^ " = " ^ print_type t2)) equations

and print_equation (e1, e2) =
  print_endline ("(" ^ print_type e1 ^ " = " ^ print_type e2 ^ ")")

and print_equations equations =
  match equations with
  | [] -> ()
  | eq :: rest -> print_equation eq; print_equations rest
;;

