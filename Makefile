# - The -I flag introduces sub-directories
# - -use-ocamlfind is required to find packages (from Opam)
# - _tags file introduces packages, bin_annot flag for tool chain
# - using *.mll and *.mly are handled automatically
# -I src -I lib # uses ocamlyacc
.PHONY: 	all clean byte native sanity test

OCB_FLAGS   = -use-ocamlfind -I src -I test -use-menhir
OCB = ocamlbuild $(OCB_FLAGS)

all: byte # profile debug

clean:
	$(OCB) -clean

native: sanity
	$(OCB) main.native

byte: sanity
	$(OCB) main.byte

sanity:
	which menhir

test:
	$(OCB) test.byte
	./test.byte