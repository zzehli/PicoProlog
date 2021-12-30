open Ast


let rec term_exp_to_string s = match s with
  | IntConst i -> "IntConst "^ string_of_int i
  | FloatConst f -> "FloatConst " ^ string_of_float f
  | VarExp v -> "VarExp " ^ v
  | CompoundTerm (a, lst) -> 
    "CompoundTerm " ^ "(" ^ a ^ ", ["^ (String.concat "; " (List.map term_exp_to_string lst))^"])"

    (* [] -> "CompoundTerm " ^ "(" ^ a ^ ", [])" *)