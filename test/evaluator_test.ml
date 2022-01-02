open OUnit2
open Evaluator
open Lib

let evaluator_test_contains =

  "contains">:::
  (List.map
    (fun (arg,res) ->
      let (term, var) = arg in 
      let title =
        (term_exp_to_string term) ^ " " ^ var
      in
        title >::
        (fun test_ctxt -> 
          assert_equal res (contains term var)))
          [ 
            (
              CompoundTerm ("x",[])
              ,
              "X"
            ), false;
            (CompoundTerm ("X",[]), "X"), false;
            ((VarExp "X"), "X"), true;
            (CompoundTerm ("family",[VarExp "X"]), "X"), true;
            (CompoundTerm ("rule",[
              CompoundTerm ("cat",[]); VarExp "X"
              ]), "X"
            ), true
          ])

let evaluator_test_subst =
  "substitute">:::
  (List.map
    (fun (arg,res) ->
      let (var, sub, exp) = arg in 
      let title =
        term_exp_to_string exp
      in
        title >::
        (fun test_ctxt ->
          assert_equal res (substitute (var, sub) exp)))
          [
            (VarExp "X", 
            VarExp "T", 
            VarExp "X"
            ), VarExp "T";
            (
              VarExp "X", 
              CompoundTerm ("Y", [CompoundTerm ("cat", [])]), 
              VarExp "X"
            ), CompoundTerm ("Y", [CompoundTerm ("cat", [])]);
            (VarExp "X", 
            IntConst 5, 
            CompoundTerm ("fish", [CompoundTerm ("cat", []); VarExp "X"; VarExp "X"])), 
            CompoundTerm ("fish", [CompoundTerm ("cat", []); IntConst 5; IntConst 5])
          ])

let evaluator_test_app_subst = 
  "apply_subst" >:::
  (List.map
  (fun (arg,res) ->
    let (sigma, term) = arg in 
    let title =
      term_exp_to_string term
    in
      title >::
      (fun test_ctxt ->
        assert_equal res (apply_subst sigma term)))
        [
          ([(VarExp"X", IntConst 6); (VarExp "Y", FloatConst 9.0)],
          CompoundTerm ("fish", [CompoundTerm ("cat", []); VarExp "X"; VarExp "Y"])),
          CompoundTerm ("fish", [CompoundTerm ("cat", []); IntConst 6; FloatConst 9.0]);
          ([(VarExp"X", CompoundTerm ("cat", [])); (VarExp "Y", FloatConst 9.0)], CompoundTerm ("X", [CompoundTerm ("cat", []); VarExp "X"])),
          CompoundTerm ("X", [CompoundTerm ("cat", []); CompoundTerm ("cat", [])])
        ])

let evaluator_test_unify = 
  "unification" >::: 
  (List.map
  (fun (arg,res) ->
    let title =
      (match arg with (x, y)::lst ->
      term_exp_to_string x ) 
    in
      title >::
      (fun test_ctxt ->
        assert_equal res (unify arg)))
        [ 
          [(VarExp "X", VarExp "X")],
          Some [];
          [
            (
              CompoundTerm("sibling", [VarExp "X"; VarExp "Y"]), 
              CompoundTerm("sibling", [VarExp "K"; VarExp "L"])
            )
          ], 
          Some [(VarExp "Y", VarExp "L");(VarExp "X", VarExp "K")];
          [(CompoundTerm("banana", [VarExp "X"]), VarExp "Y")],
          Some [(VarExp "Y", CompoundTerm("banana", [VarExp "X"]))];
          [
            (VarExp "alpha", CompoundTerm("f", [VarExp "x"])); 
            (
              CompoundTerm("g", [VarExp "alpha"; VarExp "alpha"]), 
              CompoundTerm("g", [VarExp "alpha"; VarExp "beta"])
            )
          ], Some [
                    (VarExp "alpha", CompoundTerm("f", [VarExp "x"])); 
                    (VarExp "beta", CompoundTerm("f", [VarExp "x"]))
                  ]
        ]
  )




