open Ast
open Lexing
open Parser
(*tokens to string*)
let tokens_to_string t = match t with
    | INT    i -> "INT "      ^ string_of_int i
    | FLOAT  f -> "FLOAT "    ^ string_of_float f
    | ATOM   a -> "ATOM "     ^ a 
    | VAR    v -> "VAR "      ^ v
    | RULE     -> "RULE"
    | QUERY    -> "QUERY"
    | PERIOD   -> "PERIOD"
    | LPAREN   -> "LPAREN"
    | RPAREN   -> "RPAREN"
    | COMMA    -> "COMMA"
    | EOF      -> "EOF"

let tlist_to_string tlst = "[" ^ String.concat "; " (List.map tokens_to_string tlst) ^ "]"

(*AST to string*)
let rec term_exp_to_string s = match s with
  | IntConst i -> "IntConst "^ string_of_int i
  | FloatConst f -> "FloatConst " ^ string_of_float f
  | VarExp v -> "VarExp " ^ v
  | CompoundTerm (a, lst) -> 
    "CompoundTerm " ^ "(" ^ a ^ ", ["^ (String.concat "; " (List.map term_exp_to_string lst))^"])"

let rec exp_to_string = function 
  | ClauseExp (t1, tlst) -> term_exp_to_string t1 ^":- ["^(String.concat "; " (List.map term_exp_to_string tlst))^"]"
  | QueryExp tlst -> "?- ["^(String.concat "; " (List.map term_exp_to_string tlst))^"]"


let colnum pos =
    (pos.pos_cnum - pos.pos_bol) - 1
  
let pos_string pos =
    let l = string_of_int pos.pos_lnum
    and c = string_of_int ((colnum pos) + 1) in
    "line " ^ l ^ ", column " ^ c

(* interact with the lexer and the parser*)
let parse s =
    let lexbuf = Lexing.from_string s in
    try
      let ast = Parser.main Lexer.token lexbuf in ast
    with Parser.Error ->
      raise (Failure("Parser error at "^ (pos_string lexbuf.lex_curr_p)))

(*
  Apply a substitution sigma to term T, sigma = {X1 = t1, X2 = t2, ... Xn = tn}, where Xi are variables in T and ti are terms to substitute variables. The result of performing substitution T sigma is T', an instance of T.
*)
(*
Substitutions are of the form: sigma = {X1 = t1, X2 = t2, ... Xn = tn}, where Xi are variables in T and ti are terms to substitute variables.
*)
type substitution = (term_exp * term_exp) list

let subsitute var sub : term_exp = raise (Failure "hi")

let rec contains term var : bool = match term with CompoundTerm (at, lst) -> 
                                    List.fold_right (fun elem tf -> if tf then tf else contains elem var) lst false    
                                    | VarExp m -> m = var
                                    | _ -> false

let apply_subst sigma var : term_exp = raise (Failure "hi")
let rec unify termlst : substitution option = 
    let rec addNewEqs lst1 lst2 acc =
      match lst1, lst2 with
        [], [] -> Some acc
        | t::tl, t'::tl' -> addNewEqs tl tl' ((t, t')::acc)
        | _ -> None
    in
    match termlst with
      (* Delete *)
      (s, t)::eqs when s = t -> unify eqs
      (* Eliminate *)
      | (VarExp(n), t)::eqs when not (contains t n) ->
            let eqs' = List.map (fun (t1, t2) -> subsitute (n, t) t1, subsitute (n,   t) t2) eqs
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