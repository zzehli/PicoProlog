open OUnit2

let lexer_test =
  "Lexer">:::
  (List.map
    (fun (arg,res) ->
      let title =
        arg
      in
        title >::
        (fun test_ctxt ->
          assert_equal res (Lexer.get_all_tokens arg)))
      ["x",  [ATOM "x"];
      "-+-", [ATOM "-+-"];
      "'First Rule'", [ATOM "'First Rule'"];
      "-19.4e-10", [FLOAT (-1.94e-09)];
      "-1E-10", [FLOAT (-1E-10)];
      "/*hi there*/", [];
      "%hi there", [];
      "X", [VAR "X"];
      "_case", [VAR "_case"]
      ])