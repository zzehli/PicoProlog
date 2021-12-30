{
open Parser;;

exception EndInput
}

let sign = ['+' '-']
let numeric = ['0' - '9']
let lowercase = ['a' - 'z']
let uppercase = ['A' - 'Z']
let symbol = ['#' '+' '-' '.' ':' '<' '=' '>' '?' '@' '^' '~' '$' '&' ' ']

let letter = lowercase | uppercase
let all_character = numeric | lowercase | uppercase | symbol
let float = sign ? numeric+ '.' numeric+ (['E' 'e'] sign ? numeric+) ? |
            sign ? numeric+ ['E' 'e'] sign ? numeric+

(* https://eclipseclp.org/doc/userman/umsroot143.html *)
rule token = parse
  | [' ' '\t' '\n'] { token lexbuf } 
  | eof             { EOF } 
  | ":-"            { RULE }
  | "?-"            { QUERY }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | ','             { COMMA }
  | '.'             { PERIOD }

  | sign ? numeric+ as i                      { INT (int_of_string i) }
  | "0b" ['0' '1']+ as b                      { INT (int_of_string b) }
  | "0x" ['0'- '9' 'a' - 'f']+ as x           { INT (int_of_string x) }
  | "0o" ['0'- '7']+ as o                     { INT (int_of_string o) }
  | sign ? numeric+ '.' numeric+ as f         { FLOAT (float_of_string f) }
  | float as f                                { FLOAT (float_of_string f) }
  | (uppercase | '_') ['a' - 'z' 'A' - 'Z' '_' '0' - '9']* as u { VAR u }
  | lowercase ['a' - 'z' 'A' - 'Z' '_' '0' - '9']* as a { ATOM a }
  | ''' all_character* '''  as al             { ATOM al }                
  | '%'_*'\n'                                 { token lexbuf }
  | "/*"                                      { mline_comment 0 lexbuf }
  | "*/"                                      { raise (Failure "unmatched closed comment")}

and mline_comment n = parse
  | _                            { mline_comment n lexbuf }
  | "/*"                         { mline_comment (n+1) lexbuf }
  | eof                          { raise (Failure "unmatched open comment")}
  | "*/"                         { if n > 0 then mline_comment (n-1) lexbuf else token lexbuf}
  

    

{
 let get_all_tokens s =
     let b = Lexing.from_string (s^"\n") in
     let rec g () = 
     match token b with EOF -> []
     | t -> t :: g () in
     g ()

let try_get_all_tokens s =
    try (Some (get_all_tokens s), true)
    with Failure "unmatched open comment" -> (None, true)
       | Failure "unmatched closed comment" -> (None, false)
 }

