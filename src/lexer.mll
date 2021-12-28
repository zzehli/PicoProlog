{
open Parser;;

exception EndInput
}

let sign = ['+' '-']
let numeric = ['0' - '9']
let lowercase = ['a' - 'z']
let uppercase = ['A' - 'Z']
let letter = ['a' - 'z' 'A' - 'Z']

(* https://eclipseclp.org/doc/userman/umsroot143.html *)
rule token = parse
  | [' ' '\t' '\n'] { token lexbuf } 
  | eof             { EOF } 
  | ":-"            { RULE }
  | "?-"            { QUERY }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | ','             { CONJUNCTION }
  | '.'             { PERIOD }

  | sign ? numeric+ as i                      { INT (int_of_string i) }
  | "0b" ['0' '1']+ as b                      { INT (int_of_string b) }
  | "0x" ['0'- '9' 'a' - 'f']+ as x           { INT (int_of_string x) }
  | "0o" ['0'- '7']+ as o                     { INT (int_of_string o) }
  | numeric+ '.' numeric+ as f                { FLOAT (float_of_string f) }
  | numeric+ '.' numeric+ ['E' 'e'] numeric+ as e { FLOAT (float_of_string e) }
  | lowercase+ ['a' - 'z' '_' '0' - '9']* as a { ATOM a }
  (* | '''                                      { atom lexbuf } *)
  

    

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

