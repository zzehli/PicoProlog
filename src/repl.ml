open Lib
open Parser
open Ast
open Evaluator
open Printf


(*given a clauseExp and an existing, return updated db *)
let eval_exp db = function
  ClauseExp (e, elst) as cl-> cl::db
  | QueryExp elst -> eval_query elst db; db


let rec repl db = 
  print_string "> ";
  let input = read_line() in
  let res = parse input in 
  let itr = eval_exp db res in 
  repl itr
