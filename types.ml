open Ast

(* Types simples*)
type ptype = 
  | TVar of string
  | Arr of ptype * ptype
  | N of int
  | TList of ptype   
  | Forall of string list * ptype 
;;

(* print des types *)
let rec print_type (t : ptype) : string = 
  match t with
  | TVar x -> x
  | Arr (t1,t2) -> "("^ (print_type t1)^" -> "^ (print_type t2) ^")"
  | N x -> "N(" ^ string_of_int x ^ ")"
  | TList t -> "[" ^ (print_type t) ^ "]"
  | Forall (vars, t) ->
    let vars_str = String.concat ", " vars in
    "∀" ^ vars_str ^ ". " ^ (print_type t)
;;
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



(* Récupérer les variables libres qui ne sont pas dans l'environnement *)
let rec generaliser (t : ptype) (env : env) : ptype =
  (* Fonction pour collecter les variables de type dans un type *)
  let rec collect_vars ty acc =
    match ty with
    | TVar x -> if List.mem x acc then acc else x :: acc
    | Arr (t1, t2) -> collect_vars t1 (collect_vars t2 acc)
    | TList t -> collect_vars t acc
    | N _ -> acc
    | Forall (vars, t) -> 
        let acc_filtered = List.filter (fun x -> not (List.mem x vars)) acc in
        collect_vars t acc_filtered
    | _ -> acc
  in
  (* Récupérer les variables de type présentes dans l'environnement *)
  let vars_env = List.fold_left (fun acc (_, ty) -> collect_vars ty acc) [] env in
  (* Collecter les variables libres dans le type `t` *)
  let vars_libres = collect_vars t [] in
  (* Exclure les variables de l'environnement des variables libres *)
  let vars_a_generaliser = List.filter (fun x -> not (List.mem x vars_env)) vars_libres in
  if vars_a_generaliser = [] then t
  else Forall (vars_a_generaliser, t)
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
  | Int x -> [(ty, N x)]
  | Add (x, y) | Sub (x, y) | Mul (x, y) ->
    let eq_gauche = genere_equa x (N 0) e in
    let eq_droite = genere_equa y (N 0) e in
    [(ty, N 0)] @ eq_gauche @ eq_droite
  
  | IfZero (cond, t1, t2) -> 
    let tc = genere_equa cond (N 0) e in
    let eq_t1 = genere_equa t1 ty e in
    let eq_t2 = genere_equa t2 ty e in
    tc @ eq_t1 @ eq_t2
  | IfEmpty (cond, t1, t2) ->
    let ta = TVar (nouvelle_var_t()) in
    let eq_cond = genere_equa cond (TList ta) e in
    let eq_t1 = genere_equa t1 ty e in
    let eq_t2 = genere_equa t2 ty e in
    eq_cond @ eq_t1 @ eq_t2
  
    | Let(x, e1, e2) ->
      (* Étape 1 : Inférer le type de `e1` *)
      let t0 = TVar(nouvelle_var_t()) in
      let inferred_type = inferer_type e1 e 100 in
      let t0' = match inferred_type with
        | Some ty -> ty
        | None -> failwith "Erreur lors de l'inférence du type de e1"
      in
  
      (* Étape 2 : Généraliser le type de `e1` *)
      let t0_gen = generaliser t0' e in
  
      (* Étape 3 : Ajouter `x` avec son type généralisé à l'environnement *)
      let env' = (x, t0_gen) :: e in
  
      (* Étape 4 : Générer les équations pour `e2` *)
      genere_equa e2 ty env'
  

  
  | Pfix t ->
    (* Vérifier que `t` est une abstraction *)
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

(* fonction qui verifie si une variable est dans un type *)
and occur_check (v:string) (ty:ptype) : bool = 
  match ty with
  | TVar x -> x == v 
  | Arr (t1,t2) -> occur_check v t1 || occur_check v t2
  | TList t -> occur_check v t
  | Forall (vars, t) ->
      if List.mem v vars then false
      else occur_check v t
  | N _ -> false
  | _ -> false


and substitue (v:string) (replace_with : ptype) (t:ptype) : ptype=
  match t with 
  |TVar x -> if x = v then replace_with else t 
  | N x -> N x
  |Arr(t1,t2) -> Arr((substitue v replace_with t1),(substitue v replace_with t2))
  | TList t' -> TList (substitue v replace_with t')
  | Forall (vars, t') ->
      if List.mem v vars then t
      else Forall (vars, substitue v replace_with t')
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
  | [] -> subs  (* On retourne toutes les substitutions accumulées *)
  | (t1, t2) :: rest -> 
      if t1 = t2 then unification rest subs
      else 
        match (t1, t2) with
        | N x, N y -> unification rest subs
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
        
        | _ -> failwith "échec (types incompatibles)"


and appliquer_substitution (equations : equa) (t : ptype) : ptype =
  match t with
  | TVar v -> (
      try
        (* Trouver la substitution pour `TVar v` *)
        let t' = List.assoc (TVar v) equations in
        appliquer_substitution equations t'  (* Suivre récursivement la substitution *)
      with Not_found -> TVar v
    )
  | Arr (t1, t2) -> Arr (appliquer_substitution equations t1, appliquer_substitution equations t2)
  | TList t' -> TList (appliquer_substitution equations t')
  | Forall (vars, t') ->
      (* Appliquer les substitutions uniquement aux parties non généralisées *)
      let filtered_equations = List.filter (fun (TVar x, _) -> not (List.mem x vars)) equations in
      Forall (vars, appliquer_substitution filtered_equations t')
  | _ -> t


(* fonction qui utilise l'unification pour résoudre les equations *)
and resoudre_equations equations limit = 
  try Some (unification equations [])
  with _ -> None

and trouver_variable_originale (equations : equa) (t : ptype) : ptype =
  match t with
  | TVar _ ->
      (* Rechercher une clé dans les équations dont la valeur est équivalente à `t` *)
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

