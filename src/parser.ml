type token =
    | INT of (int)
    | FLOAT of (float)
    | ATOM of (string)
    | VAR of (string)
    | QUERY
    | SEMICOLON
    | RULE
    | PLUS
    | MINUS
    | LPAREN
    | RPAREN
    | CONJUNCTION
    | PERIOD
    | EOF
