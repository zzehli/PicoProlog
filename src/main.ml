open Lib
open Ast

let test_ast_to_string alst = List.map (fun s -> print_string((exp_to_string s)^"\n")) alst

let _ =
  (* print_string (tlist_to_string (Lexer.get_all_tokens "sibling(X, Y) :- parent_child(Z, X), parent_child(Z, Y).")); *)
  print_string ((exp_to_string (parse "?- sibling(X, Y), parent_child(Z, X), parent_child(Z, Y)."))^"\n")
  
  (* test_ast_to_string [parse "cat :- 5."] *)
  (* test_ast_to_string [
    ClauseExp (CompoundTerm ("x",[]),[]);
    ClauseExp (CompoundTerm ("cat",[VarExp "X"]),[]);
    ClauseExp (CompoundTerm ("cat", []), [CompoundTerm ("true", [])]);
    QueryExp [CompoundTerm ("cat", [VarExp "X"])];
    ClauseExp (CompoundTerm ("sibling", [VarExp "X"; VarExp "Y"]), [CompoundTerm ("parent_child", [VarExp "Z"; VarExp "X"]); CompoundTerm ("parent_child", [VarExp "Z"; VarExp "Y"])])

  ] *)
  (* print_string( (term_exp_to_string (CompoundTerm ("x", [])))^"\n");
  print_string( (term_exp_to_string (CompoundTerm ("x", [IntConst 5])))^"\n");  
  print_string( (term_exp_to_string (CompoundTerm ("x", [FloatConst 5.0])))^"\n");
  print_string( (term_exp_to_string (CompoundTerm ("x", [VarExp "Y"])))^"\n");
  print_string( (term_exp_to_string (CompoundTerm ("x", [VarExp "Case"; CompoundTerm ("x", [FloatConst 5.0])])))^"\n");    
  print_string( (exp_to_string ((ClauseExp (CompoundTerm ("cat", []), [CompoundTerm ("true", [])]))))^"\n") *)