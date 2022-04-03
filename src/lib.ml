open Ast
open Lexing
open Parser


(* interact with the lexer and the parser*)
let parse s =
    let lexbuf = Lexing.from_string s in
    try
      let ast = Parser.main Lexer.token lexbuf in ast
    with Parser.Error ->
      raise (Failure("Parser error"))

(* given an expression, output its head *)
let clause_hd exp = 
  match exp with ClauseExp(a, alist) -> a
                | _ -> raise (Failure "wrong headcall input")

let clause_tl exp =
  match exp with ClauseExp(a, alist) -> alist
                | _ -> raise (Failure "wrong body call input")

(* iterate through the list and output a unit *)
let itr f list = List.fold_left (fun acc x -> f x) () list
(* ------------------------- PRINTING METHODS--------------------------------*)

(* tokens to string*)
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

(*AST to rest*)
let rec term_exp_to_res t = match t with
 IntConst i -> string_of_int i
 | FloatConst f -> string_of_float f
 | VarExp v -> v
 | CompoundTerm (a, lst) -> 
                  (match lst with 
                    [] -> a
                    | x::xs -> a ^ "( " ^ (
                                            String.concat "; " (
                                              List.map term_exp_to_res lst
                                              )
                                          )^ ")")
    
let rec exp_to_string = function 
  | ClauseExp (t1, tlst) -> 
    term_exp_to_string t1 ^":- ["^(
      String.concat "; " (List.map term_exp_to_string tlst)
      )^"]"
  | QueryExp tlst -> "?- ["^(
      String.concat "; " (List.map term_exp_to_string tlst)
      )^"]"

(*subst for testing*)
let subst_to_string lst = 
(match lst with [] -> "[]" 
| x::xs -> List.fold_left (fun str elem -> let (x, y) = elem in str ^(term_exp_to_string x) ^ "|--> " ^ (term_exp_to_string y )^ ";; ")  "" lst )

(*subst for printing results*)
let subst_to_res lst =
match lst with [] -> "None"
      | x::xs -> List.fold_left (fun str elem -> let (x, y) = elem in str ^ (term_exp_to_res x) ^ " = " ^ (term_exp_to_res y) ^ "\n") "" lst
let unify_to_string = function
  | Some lst -> subst_to_string lst
  | None -> "None"

let match_rules_to_string_aux elem = match elem with
 (subst, cl) ->
    let sub_lst = subst_to_string subst in
    "(" ^ sub_lst ^ exp_to_string cl ^ ")\n"
  | _ -> raise (Failure "wrong input to matched to string")

let match_rules_to_string lst =
   List.fold_left (fun acc elem -> acc ^ (match_rules_to_string_aux elem))
                  "" 
                  lst