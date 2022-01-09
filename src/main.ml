open Lib
open Ast
open Evaluator

let () =
  let rec loop () = (
    print_string "?-";
    let input = read_line() in
    print_string (input^", correct?");
    loop()
  )
  in
    loop ()
(* 
let test_ast_to_string alst = List.map (fun s -> print_string((exp_to_string s)^"\n")) alst
let gl = [CompoundTerm("anc", [VarExp "X"; CompoundTerm ("bart", [])])]
let subst = compose
[(CompoundTerm("homer", []), VarExp "X1"); (CompoundTerm("bart", []), VarExp "Y1" )]
[(VarExp "X", VarExp "X1"); (CompoundTerm("bart", []), VarExp "Y1")]

let _ =
    (eval_query_solv 
    [CompoundTerm("b", [VarExp "K"; VarExp "Y"])]
    [CompoundTerm("b", [VarExp "K"; VarExp "Y"])]
    [
          ClauseExp (CompoundTerm("a", [VarExp "X"]), []);
          ClauseExp (CompoundTerm("b", [VarExp "X"; VarExp "Y"]), []);
          ClauseExp (
            CompoundTerm("b", [VarExp "Z"; VarExp"Y"]), 
            [CompoundTerm("meow", [VarExp "X"; VarExp "Y"])]
          )
    ]
    []
    ) *)
      (* subst_to_string(List.filter (fun (x, y) -> contain_lst gl x) subst) *)
    
      (* subst_to_string(
      compose
        [(CompoundTerm("homer", []), VarExp "X1"); (CompoundTerm("bart", []), VarExp "Y1" )]
        [(VarExp "X", VarExp "X1"); (CompoundTerm("bart", []), VarExp "Y1")]
      ) *)
      (* List.fold_left (fun acc elem -> (term_exp_to_string elem )^acc) ""
      (apply_subst_lst 
        [(VarExp "X", VarExp "X1"); (CompoundTerm("bart", []), VarExp "Y1")]
        [CompoundTerm ("parent", [VarExp "X1"; VarExp "Y1"])]) *)
    
    
  
  (* print_string( exp_to_string(
    rename (fresh()) (ClauseExp (CompoundTerm ("cat", []), 
                              [VarExp("X"); VarExp("Y")]))
  )) *)
  
  (* print_string(string_of_bool(
    contain_lst 
    [CompoundTerm ("fish",[VarExp "Y"; CompoundTerm ("fish",[VarExp "Y"; VarExp "Z"]); VarExp "X"])]
    (VarExp "X")
    )^"\n") *)
  (* print_string(
    match_rules_to_string (
      match_rules [
        ClauseExp (CompoundTerm("a", [VarExp "X"]), []);
        ClauseExp (CompoundTerm("b", [VarExp "X"; VarExp "Y"]), []);
        ClauseExp (
          CompoundTerm("b", [VarExp "Z"; VarExp"Y"]), 
          [CompoundTerm("meow", [VarExp "X"; VarExp "Y"])]
        )
      ]
      (CompoundTerm("b", [VarExp "K"; VarExp "Y"]))
    )
  ) *)
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