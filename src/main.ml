open Lib
open Ast
open Evaluator

let test_ast_to_string alst = List.map (fun s -> print_string((exp_to_string s)^"\n")) alst

let _ =
  (* print_string( exp_to_string(
    rename (fresh()) (ClauseExp (CompoundTerm ("cat", []), 
                              [VarExp("X"); VarExp("Y")]))
  )) *)
  print_string(
    match_rules_to_string (
      match_rules [ClauseExp (CompoundTerm ("cat", [VarExp "X"]), [])]
      (CompoundTerm("cat", [VarExp "Y"]))
    )
  )
  (* 
let _ =
  print_string (unify_to_string(Some [(VarExp "X", VarExp "K");(VarExp "Y", VarExp "L")])^"\n");
  print_string (unify_to_string(unify([(CompoundTerm("sibling", [VarExp "X"; VarExp "Y"]), CompoundTerm("sibling", [VarExp "K"; VarExp "L"]))]))^"\n");
  print_string (unify_to_string(unify([(VarExp "X", VarExp "X")]))^"\n");
  
  print_string (unify_to_string(unify(
    [
      (VarExp "alpha", CompoundTerm("f", [VarExp "x"])); 
      (CompoundTerm("g", [VarExp "alpha"; VarExp "alpha"]), CompoundTerm("g", [VarExp "alpha"; VarExp "beta"]))
    ]))^"\n") *)


  (* print_string (tlist_to_string (Lexer.get_all_tokens "sibling(X, Y) :- parent_child(Z, X), parent_child(Z, Y).")); *)
  (* print_string ((exp_to_string (parse "cat(tom). animal(X) :- cat(X). ?- animal(X)."))^"\n") *)
  
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