open Lexer
open Lexing

let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.main Lexer.token lexbuf in
  ast
  
let _ =
  parse "hi."