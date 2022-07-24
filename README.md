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

# Sample Run
Here is a sample session:
```
prolog (main) ❱ ./main.byte 
> ancestor(X, Y) :- parent(X, Y).
> parent(abe, homer).
> parent(abe, herbert).
> parent(homer, bart).
> ?- ancestor(X, bart).
X = homer
> parent(marge, bart).
> ?- ancestor(X, bart).
X = marge
X = homer
> ancestor(X, Y) :- parent(Z, Y), ancestor(X, Z).
> ?- ancestor(X, bart).
X = abe
X = marge
X = homer
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
# Prolog
Prolog is a logic programming language that describes objects and their relationships. Here, the object is different from object in the object-oriented programs where objects carry methods and fields, but pieces of facts or evidence to form a web of knowledge (or relations). Prolog use logic to construct new facts based on given ones.

There are three kind of expressions in Prolog:
- *facts* about objects and their relationships
- *rules* about objects and their relationships; these can be thought of as rules that desribe how to make new facts from given ones
- *questions* about objects and their relationships

# BNF Grammar
This is where the concrete grammar and the abstract syntax comes together. We first list the BNF grammar and then describe how it describes the three kinds of expressions in Prolog: rule, fact, and question. PicoProlog's BNF grammar is the following rules:
```
<program> ::= <head> ":-" <term_list> "." | "?-" <term_list> "." | <term> "."
<term_list> ::= <term> | <term> "," <term_list>
<head> ::= ATOM | ATOM "(" <term_list> ")"
<term> ::= INT | FLOAT | VAR | ATOM | ATOM "(" <term_list> ")"
```

The concrete syntax of PicoProlog is based on the ISO Prolog and ECLiPSe Prolog. In prolog, the only data type is `term`, which is comprised of number, variable, atom, and compound term. Therefore, the central task is to define the syntax for term. Besides term, we also define special symbols such as `:-` and `?-`, which are elements connecting terms to form rules and queries.

The first rule in the BNF grammar above states that there are three kinds of statements accepted by PicoProlog, differentiated by the special symbols `:-` and `?-`. **Facts** (or predicates) are true statement the require no further action. It's syntax involves none of the two symbols. **Rules** consist of a head and a body connected by a RULE symbol, `:-`. The head can be seen as a fact with no body. If the fact include variables as parameters, the variables are further defined in the body, which is a list of facts connected by comma, the conjunction relation. A full implementation of prolog allows other logic relations such as implication, disjunction, etc. A collection of facts and rules is called database. For a given database, Prolog uses **queries** to ask questions. Queries starts with `-?`.

# Reference
## Documents and Books
unknow, Appendix A: An Introduction to Prolog  
Clocksin and Mellish, Programming in Prolog  
Spivey, An introduction to logic programming through Prolog  
Deransart, Ed-Dbali and Cervoni, Prolog: The Standard  
Sterling and Shapiro, The Art of Prolog  
Boizumault, The Implementation of Prolog
# Other implementations
SICStus Prolog  
Eclipse Prolog  
miniprolog on Programming Zoo: https://plzoo.andrej.com/language/miniprolog.html  
tiny prolog in OCaml: https://naereen.github.io/Tiny-Prolog-in-OCaml/

