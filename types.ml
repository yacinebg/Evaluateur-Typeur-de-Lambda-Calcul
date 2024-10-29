open Ast

(* Types simples*)
type ptype = TVar of string
  | Arr of ptype * ptype
  | Nat
;;

(* print des types *)
let rec print_type (t : ptype) : string = 
  match t with
  | TVar x -> x
  | Arr (t1,t2) -> "("^ (print_type t1)^" -> "^ (print_type t2) ^")"
  | _ -> ""
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
;;


(* fonction qui verifie si une variable est dans un type *)
let rec occur_check (v:string) (ty:ptype) : bool = 
  match ty with
  | TVar x -> x == v 
  | Arr (t1,t2) -> occur_check v t1 || occur_check v t2
  | _ -> false
;;


let rec substitue (v:string) (replace_with : ptype) (t:ptype) : ptype=
  match t with 
  |TVar x -> if x = v then replace_with else t 
  |Arr(t1,t2) -> Arr((substitue v replace_with t1),(substitue v replace_with t2))
  | _ -> Nat
;;

let rec substitute_in_equations (v : string) (replacement : ptype) (equations : equa) : equa =
  match equations with
  | [] -> []
  | (t1, t2) :: rest ->
      let new_t1 = substitue v replacement t1 in
      let new_t2 = substitue v replacement t2 in
      (new_t1, new_t2) :: substitute_in_equations v replacement rest


(* fonction qui implémente l'algorithme d'unification des fonctions *)
let rec unification (equations : equa) (subs : equa) : equa =
  match equations with
  | [] -> subs  (* On retourne toutes les substitutions accumulées *)
  | (t1, t2) :: rest -> 
      if t1 = t2 then unification rest subs
      else 
        match (t1, t2) with
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
        | _ -> failwith "échec (types incompatibles)"
;;


let print_equation (e1, e2) =
  print_endline ("(" ^ print_type e1 ^ " = " ^ print_type e2 ^ ")")
;;

let rec print_equations equations =
  match equations with
  | [] -> ()
  | eq :: rest -> print_equation eq; print_equations rest
;;

(* fonction qui utilise l'unification pour résoudre les equations *)
let resoudre_equations equations limit = 
  try Some (unification equations [])
  with _ -> None
;;

let rec appliquer_substitution (equations : equa) (t : ptype) : ptype =
  match equations with
  | [] -> t
  | (TVar v, t') :: rest -> appliquer_substitution rest (substitue v t' t)
  | _ -> t
;;

let rec print_substitutions (equations : equa) =
  List.iter (fun (t1, t2) -> print_endline (print_type t1 ^ " = " ^ print_type t2)) equations
;;

let inferer_type (term : pterm) (env : env) (limit : int) : ptype option =
  let t = TVar (nouvelle_var_t()) in
  let equations = genere_equa term t env in
  match resoudre_equations equations limit with
  | None -> print_endline "echec de l'unification des equations"; None
  | Some eqs -> 
    let final_type = appliquer_substitution eqs t in
    Some final_type
;;