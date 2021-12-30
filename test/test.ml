open OUnit2
open Lexer_test
open Parser_test
(* Doc for OUnit2: https://gildor478.github.io/ounit/ounit2/index.html *)

let () =
  run_test_tt_main lexer_test;
  run_test_tt_main parser_test


  (* module AST = Test_basic_ast.UnitTests;;
  module Typecheck = Test_typechecker.UnitTests;;
  module Frame_annotate = Test_frame_notes.UnitTests;;
  module Erase = Test_erased_ast.UnitTests;;
  module MapRep = Test_map_replicate_ast.UnitTests;;
  module Clos = Test_closures.UnitTests;;
  open OUnit2
  
  let () =
    run_test_tt_main AST.suite_init_drop;
    run_test_tt_main Typecheck.tests;
    run_test_tt_main Frame_annotate.tests;
    run_test_tt_main Erase.tests;
    run_test_tt_main MapRep.tests;
    run_test_tt_main Clos.tests
  ;; *)