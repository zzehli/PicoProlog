%{
    open Ast
%}

%token <int> INT
%token <float> FLOAT
%token <string> VAR ATOM
%token QUERY RULE SEMICOLON PLUS MINUS LPAREN RPAREN COMMA PERIOD EOF


%type <Ast.exp> main
%start main

%%

main:
 expression RULE expression_list PERIOD   { ClauseExp ($1, $3) }
 | QUERY expression_list PERIOD           { QueryExp $2 }
 | expression PERIOD                 { ClauseExp ($1, [])}

expression_list:
 | expression                            { [$1] }
 | expression COMMA expression_list      { $1 :: $3  }

expression:
 | INT                               { IntConst $1 }
 | FLOAT                             { FloatConst $1 }
 | VAR                               { VarExp $1 }
 | ATOM LPAREN expression_list RPAREN  { CompoundTerm ($1, $3)}
    
