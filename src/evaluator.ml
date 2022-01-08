open Lib
open Ast
(*
  Apply a substitution sigma to term T, sigma = {X1 = t1, X2 = t2, ... Xn = tn},
   where Xi are variables in T and ti are terms to substitute variables. 
   The result of performing substitution T sigma is T', an instance of T.
*)
(*
Substitutions are of the form: sigma = {X1 = t1, X2 = t2, ... Xn = tn}, where
Xi are variables in T and ti are terms to substitute variables.
*)
type substitution = (term_exp * term_exp) list

let rec contains term var : bool = 
                              match term with 
                              CompoundTerm (at, lst) -> 
                                  List.fold_right (
                                      fun elem tf -> 
                                        if tf 
                                          then tf else contains elem var
                                      ) lst false    
                              | VarExp m -> m = var
                              | _ -> false
and contain_lst lst term : bool = 
    let VarExp var = term in 
    List.fold_left (fun acc elem -> 
                    if acc then acc else contains elem var) false lst
(*
substitute a given variable with a term in a given compound term
this is similar to apply_subst, but here the subsitition is {X = t} where as in apply_st the substitution is a list
*)                                    
let rec substitute (var, sub) exp: term_exp = match exp 
                  with VarExp(x) -> if var = VarExp(x) then sub else VarExp(x)
                      | CompoundTerm (at, lst) -> let lst' = 
                            List.map (
                              fun t -> substitute (var, sub) t
                              ) lst in CompoundTerm(at, lst')
                      | _ -> exp

(*
apply a substitution sigma to term
*)
let rec apply_subst sigma term : term_exp = match sigma with 
                                [] -> term
                                | x::xs -> let (VarExp(var), sub) = x in 
                                    let term' = substitute (VarExp(var), sub) term in
                                    apply_subst xs term'

(*
apply a substitution to a list
*)
let apply_subst_lst sigma tlst : term_exp list = 
                                List.map (apply_subst sigma) tlst

let apply_subst_subst s1 s2 = 
  List.map (fun (x1, x2) -> (apply_subst s1 x1, apply_subst s1 x2)) s2

(*
add a new pair to existing substitution
*)
let subst_add phi new_add : substitution = (new_add::phi)
(*
unify given list of pairs of terms
*)
let rec unify termlst : substitution option = 
    let rec addNewEqs lst1 lst2 acc =
      match lst1, lst2 with
        [], [] -> Some acc
        | t::tl, t'::tl' -> addNewEqs tl tl' ((t, t')::acc)
        | _ -> None
    in
    match termlst with
      [] -> Some([])
      (* Delete *)
      | (s, t)::eqs when s = t -> unify eqs
      (* Eliminate *)
      | (VarExp(n), t)::eqs when not (contains t n) ->
            let eqs' = 
            List.map (fun (t1, t2) -> substitute (VarExp(n), t) t1, substitute (VarExp(n),   t) t2) eqs
            in (match unify eqs' with 
              None -> None
              | Some(phi) -> Some(subst_add phi (VarExp(n), apply_subst phi t)))
      (* Orient *)
      | (s, VarExp(n))::eqs -> (unify ((VarExp(n), s)::eqs))
      (* Decompose *)
      | (CompoundTerm(at, lst), CompoundTerm(at', lst'))::eqs when at = at' ->
              (match (addNewEqs lst lst' eqs) with
                None -> None
                | Some nl -> unify nl)
      | _ -> None

(*
check if given expression is fact, which is a clause without body
*)
let is_fact exp : bool = match exp with 
                      ClauseExp (term, lst) when lst = [] -> true
                      | _ -> false

let term_hd exp : term_exp = match exp with ClauseExp(x, lst)  -> x
                              | _ -> raise (Failure "Wrong expression: no head")

let term_tl exp : term_exp list = match exp with ClauseExp(x, lst) -> lst
                              | _  -> raise (
                                Failure "Wrong expression: no tail"
                                )
(*fresh var*)
let (fresh, reset) =
  let nxt = ref 0 in
  let f () = (nxt := !nxt + 1; string_of_int (!nxt)) in
  let r () = nxt := 0 in
  (f, r)

(* rename an expression *)
let rec rename suffix = function
   ClauseExp (exp, lst) -> ClauseExp(rename_term suffix exp, List.map (rename_term suffix) lst)
  | _ as e -> e
and rename_term suffix = function
    VarExp(a) -> VarExp(a ^ suffix)
    | CompoundTerm(a, alist) -> CompoundTerm(
                                a,  
                                List.map (rename_term suffix) alist)
    | _ as e -> e

(* Given a term, and a list of rules, find if the term unifies with any of the
  term unifies with any of the (renamed) rules, if so save the substitution, 
    then return the list of substitutions and corresponding rules *)
let rec match_rules db hd = 
  match db with [] -> []
          | x::xs -> let x' = rename (fresh()) x in
                     match(unify [(hd, clause_hd x')] ) with
                     None -> match_rules xs hd
                     | Some sigma -> (sigma, x')::match_rules xs hd

let rec in_list x = function  (* List.mem *)
  | [] -> false
  | a::q -> a=x || (in_list x q)

let rec filter l = function  (* List.filter *)
  | [] -> []
  | (x, t)::q -> if in_list x l then (filter l q) else (x, t)::(filter l q)

let subst_on_couple x t = function (p, trm) -> (p, substitute (x, t) trm)

let rec apply_subst_on_subst s1 s2 =
  match s1 with
  | [] -> s2
  | (x, t)::q -> (apply_subst_on_subst q (List.map (subst_on_couple x t) s2))
  
let compose s1 s2 = (filter (List.map fst s2) s1)@(apply_subst_on_subst s1 s2)

(*
gl: original goal list: term_exp list
svt: solvent list, current goal list: term_exp list
db: database: exp list
*)
let rec eval_query_solv gl svt db subst = match svt with
  [] -> 
    print_string(
      subst_to_string(List.filter (fun (x, y) -> contain_lst gl x) subst)
    )
    (*if solvent has 0 element, output current substitution*)
  | x::xs -> 
    let u = match_rules db x in
    itr 
      (
        fun (s, cl)  -> (eval_query_solv 
                        gl
                        (apply_subst_lst s ((clause_tl cl)@xs))
                        db
                        (compose s subst)
      ))
      u
(*1. contain_list
  2. unification instead of apply
  *)

(*
gl
evaluate a query input against a database
*)
(* let eval_query gl db = eval_query_solve gl gl db subst *)

