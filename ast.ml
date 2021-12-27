type atom = string

type term_exp = IntConst of int | FloatConst of float | VarExp of string | CompoundTerm of atom * (term_exp list)

type exp = 
  | ClauseExp of term_exp * term_exp
  | QueryExp of term_exp list
  | TermExp of term_types