# PicoProlog: Prolog in OCAML
An implementation of prolog in OCaml including lexer, parser and evaluator. PicoProlog is built with Ocamlbuild and includes unit tests for each component.

# How to run
To install the project, clone it from github and build with `make` command. 
```
git clone https://github.com/zzehli/PicoProlog.git
cd PicoProlog
make
```
The `make` command will run the `Makefile` to build the executible `main.byte`, run the program with
```
./main.byte
```

# Run Tests
The testing framework is built with:
```
make test
```
which generate `test.byte`. Run the executable with
```
./test.byte
```

# Concrete Syntax
The concrete syntax of PicoProlog is based on the ISO Prolog and ECLiPSe Prolog. In prolog, the only data type is term, which is comprised of number, variable, atom, and compound term. Therefore, the central task is to define the syntax for term. Besides term, we also define comment, special symbols such as `:-` and `?-`, which are elements connecting terms to form clauses.

1. Term
	- number: integer and float; both can be signed: `12, -10, -1.0, -0.4E+02`; also include binary, octal, and hex-based integers
	- variable: a sequence of letters, digits, or underscore characters beginning with an upper case letter or an underscore `_`: `X, _a1, Var, Number_case`
	- atom: 1) a sequence of letters, digits or underscore characters beginning with a lower case letter 2) sequence of symbols 3) arbitrary characters enclosed in single quotes. Here are some examples `x1, rule_elim, '1', 'First Rule', +, -*-`
	- compound term: a compound term is a structure composed of a name, which is an atom, and a list of terms enclosed by parenthesis as its arguments: `date(december, 25, "Christmas"), element(hydrogen, composition(1,0)), +(X, 1.0)`
2. Comment: single line comments begin with `%` and multi-line comments are enclosed by `\*[content]*/`
3. Special Symbols: 
	- `:-` is called *rule*
	- `?-` is called *query*

