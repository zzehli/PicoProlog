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
                              match term with CompoundTerm (at, lst) -> 
                              List.fold_right (
                                  fun elem tf -> if tf 
                                                then tf else contains elem var
                                  ) lst false    
                              | VarExp m -> m = var
                              | _ -> false

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
              | Some(phi) -> Some((VarExp(n), apply_subst phi t)::phi))
      (* Orient *)
      | (s, VarExp(n))::eqs -> (unify ((VarExp(n), s)::eqs))
      (* Decompose *)
      | (CompoundTerm(at, lst), CompoundTerm(at', lst'))::eqs when at = at' ->
              (match (addNewEqs lst lst' eqs) with
                None -> None
                | Some nl -> unify nl)
      | _ -> None

(*
Renaming substitution
*)

(*
unification:
to solve equation AND
to match a goal or a subgoal
*)

