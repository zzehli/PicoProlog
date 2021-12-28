open OUnit2
open Lexer

(* Doc for OUnit2: https://gildor478.github.io/ounit/ounit2/index.html *)


let suite =
  "Lexer">:::
  (List.map
    (fun (arg,res) ->
      let title =
        arg
      in
        title >::
        (fun test_ctxt ->
          assert_equal res (Lexer.get_all_tokens arg)))
      ["x",  [ATOM "x"]
      ])

let () =
  run_test_tt_main suite