open Lib
open Ast
let _ = 
  print_string( (term_exp_to_string (CompoundTerm ("x", [])))^"\n");
  print_string( (term_exp_to_string (CompoundTerm ("x", [IntConst 5])))^"\n");  
  print_string( (term_exp_to_string (CompoundTerm ("x", [FloatConst 5.0])))^"\n");
  print_string( (term_exp_to_string (CompoundTerm ("x", [VarExp "Y"])))^"\n");
  print_string( (term_exp_to_string (CompoundTerm ("x", [VarExp "Case"; CompoundTerm ("x", [FloatConst 5.0])])))^"\n")    