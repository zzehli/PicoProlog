open Ast
open Lexing
open Parser


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

let rec exp_to_string = function 
  | ClauseExp (t1, tlst) -> 
    term_exp_to_string t1 ^":- ["^(
      String.concat "; " (List.map term_exp_to_string tlst)
      )^"]"
  | QueryExp tlst -> "?- ["^(
      String.concat "; " (List.map term_exp_to_string tlst)
      )^"]"

let subst_to_string = function
  | Some lst -> (match lst with [] -> "[]" 
    | x::xs -> List.fold_left (fun str elem -> let (x, y) = elem in str ^(term_exp_to_string x) ^ "|--> " ^ (term_exp_to_string y )^ ";; ")  "" lst )
  | None -> "None"

(* 
  List.fold_right (
    fun elem tf -> if tf 
                  then tf else contains elem var
    ) lst false    *)