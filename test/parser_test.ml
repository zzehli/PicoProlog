open OUnit2
open Lib
open Ast
let parser_test =
  "Lexer">:::
  (List.map
    (fun (arg,res) ->
      let title =
        arg
      in
        title >::
        (fun test_ctxt ->
          assert_equal res  (parse arg)))
          [
            "x.", ClauseExp (
                      CompoundTerm ("x",[]),[]
                      );

            "cat(X).", ClauseExp (
                          CompoundTerm ("cat",[VarExp "X"]),[]
                          );
            
            "cat :- true.", ClauseExp (
                                CompoundTerm ("cat", []), [
                                  CompoundTerm ("true", [])
                                  ]
                                );
            
            "sibling(X, Y) :- parent_child(Z, X), parent_child(Z, Y).", 
                ClauseExp (
                  CompoundTerm ("sibling", [VarExp "X"; VarExp "Y"]), [
                    CompoundTerm ("parent_child", [VarExp "Z"; VarExp "X"]); CompoundTerm ("parent_child", [VarExp "Z"; VarExp "Y"])
                    ]
                );
            
            "?- cat(X).", QueryExp [
              CompoundTerm ("cat", [VarExp "X"])
              ];
            
            "?- sibling(tom, Y), parent_child(Z, X), parent_child(Z, Y).", 
            QueryExp[
              CompoundTerm ("sibling", [CompoundTerm ("tom", []); VarExp "Y"]);
              CompoundTerm ("parent_child", [VarExp "Z"; VarExp "X"]);
              CompoundTerm ("parent_child", [VarExp "Z"; VarExp "Y"])
              ]
          ])