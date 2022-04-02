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
          (*delete*)
          [(VarExp "X", VarExp "X")],
          Some [];
          (*two compound terms; decompose*)
          [
            (
              CompoundTerm("sibling", [VarExp "X"; VarExp "Y"]), 
              CompoundTerm("sibling", [VarExp "K"; VarExp "L"])
            )
          ], 
          Some [(VarExp "Y", VarExp "L");(VarExp "X", VarExp "K")];
          (*var and compound term; orient*)
          [(CompoundTerm("banana", [VarExp "X"]), VarExp "Y")],
          Some [(VarExp "Y", CompoundTerm("banana", [VarExp "X"]))];
          (*complex unification*)
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


  let evaluator_test_match_rules = 
    "match rules" >::: 
    (List.map
    (fun (arg,res) ->
      let (db, hd) = arg in
      let title = term_exp_to_string hd 
      in
        title >::
        (fun test_ctxt ->
          assert_equal res (match_rules db hd))) [
            ( (*one rule match*)
              [ClauseExp (
                CompoundTerm ("cat", [VarExp "X"]), 
                [CompoundTerm("meow", [VarExp "X"; VarExp "Y"])]
               )
              ],
            CompoundTerm("cat", [VarExp "Y"])), 
            [
              [(VarExp "Y",VarExp "X1")],
              ClauseExp (
                CompoundTerm ("cat", [VarExp "X1"]), 
                [CompoundTerm("meow", [VarExp "X1"; VarExp "Y1"])]
              )
            ];
            (*empty match*)
            ([ClauseExp (CompoundTerm("atom", [VarExp "X"]), [])],
              CompoundTerm("atom", [VarExp "Y"; VarExp "X"])
            ),
            [];
            (*two rules out of three match*)
            (* (
              [
                ClauseExp (CompoundTerm("a", [VarExp "X"]), []);
                ClauseExp (CompoundTerm("b", [VarExp "X"; VarExp "Y"]), []);
                ClauseExp (
                  CompoundTerm("b", [VarExp "Z"; VarExp"Y"]), 
                  [CompoundTerm("meow", [VarExp "X"; VarExp "Y"])]
                )
              ],
              CompoundTerm("b", [VarExp "K"; VarExp "Y"])
            ),
            [
              (
                [ (VarExp "Y", VarExp "Y2");(VarExp "K", VarExp "X2")],
                ClauseExp (CompoundTerm("b", [VarExp "X2"; VarExp "Y2"]), [])
              );
              (
                [(VarExp "Y", VarExp "Y3");(VarExp "K", VarExp "Z3")],
                ClauseExp (
                  CompoundTerm("b", [VarExp "Z3"; VarExp"Y3"]), 
                  [CompoundTerm("meow", [VarExp "X3"; VarExp "Y3"])]
                )
              )
            ] *)

          ])

