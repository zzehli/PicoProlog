open OUnit2
open Lib

let parser_test =
  "Lexer">:::
  (List.map
    (fun (arg,res) ->
      let title =
        arg
      in
        title >::
        (fun test_ctxt ->
          assert_equal res (exp_to_string (parse arg))))
          ["x.", "CompoundTerm (x, []):- []";
          "cat(X).", "CompoundTerm (cat, [VarExp X]):- []";
          "cat :- true.", "CompoundTerm (cat, []):- [CompoundTerm (true, [])]";
          "sibling(X, Y) :- parent_child(Z, X), parent_child(Z, Y).", "CompoundTerm (sibling, [VarExp X; VarExp Y]):- [CompoundTerm (parent_child, [VarExp Z; VarExp X]); CompoundTerm (parent_child, [VarExp Z; VarExp Y])]";
          "?- cat(X).", "?- [CompoundTerm (cat, [VarExp X])]";
          "?- sibling(X, Y), parent_child(Z, X), parent_child(Z, Y).", "?- [CompoundTerm (sibling, [VarExp X; VarExp Y]); CompoundTerm (parent_child, [VarExp Z; VarExp X]); CompoundTerm (parent_child, [VarExp Z; VarExp Y])]"
          ])