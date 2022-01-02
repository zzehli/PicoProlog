open OUnit2
open Lexer_test
open Parser_test
open Evaluator_test
(* Doc for OUnit2: https://gildor478.github.io/ounit/ounit2/index.html *)

let () =
  (* run_test_tt_main lexer_test; *)
  (* run_test_tt_main parser_test; *)
  run_test_tt_main evaluator_test_contains;
  run_test_tt_main evaluator_test_subst;
  run_test_tt_main evaluator_test_app_subst;
  run_test_tt_main evaluator_test_unify