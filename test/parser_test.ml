open OUnit2

let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.main Lexer.token lexbuf in
  ast

let parser_test =
  "Lexer">:::
  (List.map
    (fun (arg,res) ->
      let title =
        arg
      in
        title >::
        (fun test_ctxt ->
          assert_equal res (parse arg)))
          ["x.", (ClauseExp (CompoundTerm ("x",[]),[]))

          ])